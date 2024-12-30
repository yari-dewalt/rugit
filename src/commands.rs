use super::RUGIT_DIR;
use core::{panic, str};
use std::error::Error;
use sha1::{Sha1, Digest};
use std::io::{Write, Read};
use std::collections::{HashMap,VecDeque,HashSet};
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::NamedTempFile;
use similar::{ChangeTag, TextDiff};
use serde_json;

pub struct Commit {
    pub tree: String,
    pub parents: Vec<String>,
    pub message: String,
}

struct RefValue {
    symbolic: bool,
    value: String,
}

pub struct IndexGuard {
    index: HashMap<String, String>,
    path: PathBuf,
}

impl IndexGuard {
    pub fn new() -> Result<Self, Box<dyn Error>> {
        let path = PathBuf::from(format!("{RUGIT_DIR}/index"));

        let index = if path.exists() {
            let mut contents = String::new();
            std::fs::File::open(&path)?.read_to_string(&mut contents)?;
            serde_json::from_str(&contents)?
        } else {
            HashMap::new()
        };

        Ok(IndexGuard { index, path })
    }

    pub fn get_index(&mut self) -> &mut HashMap<String, String> {
        &mut self.index
    }
}

impl Drop for IndexGuard {
    fn drop(&mut self) {
        if let Ok(contents) = serde_json::to_string(&self.index) {
            if let Ok(mut file) = std::fs::File::create(&self.path) {
                let _ = file.write_all(contents.as_bytes());
            }
        }
    }
}

pub fn init() -> Result<String, Box<dyn Error>> {
    let path = std::env::current_dir()?;

    if path.join(RUGIT_DIR).exists() {
        println!("Rugit repository already exists");
        return Ok(String::from("Success"))
    }

    // .rugit
    std::fs::create_dir(RUGIT_DIR)?;
    // .rugit/objects
    std::fs::create_dir(format!("{RUGIT_DIR}/objects"))?;

    let path_display = path.display();
    update_ref("HEAD", RefValue { symbolic: true, value: "refs/heads/main".to_string() }, None, None)?;
    println!("Initialized empty rugit repository in {path_display}/{RUGIT_DIR}");

    Ok(String::from("Success"))
}

pub fn hash_object(file_content: &[u8], _type: Option<&str>) -> Result<String, Box<dyn Error>> {
    let object_type = _type.unwrap_or("blob");
    let obj_data = [object_type.as_bytes(), b"\x00", file_content].concat();
    let mut hasher = Sha1::new();

    hasher.update(&obj_data);
    let result = hasher.finalize();
    let hash_hex = format!("{:x}", result);

    let path = std::env::current_dir()?;
    if !path.join(RUGIT_DIR).exists() || !path.join(format!("{RUGIT_DIR}/objects")).exists() {
        init()?;
    }

    let file_path = format!("{RUGIT_DIR}/objects/{hash_hex}");
    let mut file = std::fs::File::create(file_path)?;
    file.write_all(&obj_data)?;

    Ok(hash_hex)
}

pub fn cat_file(object_id: &str) -> Result<String, Box<dyn Error>> {
    let result = get_object(object_id, None)?;
    std::io::stdout().flush()?;
    println!("{:?}", result);
    Ok(String::from("Success"))
}

fn get_object(object_id: &str, expected: Option<&str>) -> Result<Vec<u8>, Box<dyn Error>> {
    let expected = expected.unwrap_or("blob");

    let bytes = std::fs::read(format!("{RUGIT_DIR}/objects/{object_id}"))?;

    let index = bytes.iter().position(|&byte| byte == b'\x00').unwrap();
    let (before, after) = bytes.split_at(index);
    let after = &after[1..];

    let _type = str::from_utf8(before)?;
    assert_eq!(_type, expected, "Expected {expected}, got {_type}");

    Ok(after.to_vec())
}

type Tree = HashMap<String, Node>;

enum Node {
    Directory(Tree),
    File(String),
}

pub fn write_tree() -> Result<String, Box<dyn Error>> {
    let mut index_as_tree: Tree = HashMap::new();
    let mut guard = IndexGuard::new()?;
    let index = guard.get_index();

    for (path, oid) in index {
        let path_parts: Vec<&str> = path.split('/').collect();
        let dirpath = &path_parts[..path_parts.len() - 1];
        let filename = path_parts[path_parts.len() - 1];

        let mut current = &mut index_as_tree;
        for dirname in dirpath {
            current = match current
                .entry(dirname.to_string())
                .or_insert_with(|| Node::Directory(HashMap::new()))
            {
                Node::Directory(tree) => tree,
                _ => return Err("Expected a directory but found a file".into()),
            };
        }
        current.insert(filename.to_string(), Node::File(oid.to_string()));
    }

    fn write_tree_recursive(tree_dict: &Tree) -> Result<String, Box<dyn Error>> {
        let mut entries = Vec::new();

        for (name, value) in tree_dict {
            match value {
                Node::Directory(subtree) => {
                    let _type = "tree";
                    let oid = write_tree_recursive(subtree)?;
                    entries.push((name.clone(), oid, _type.to_string()));
                }
                Node::File(oid) => {
                    let _type = "blob";
                    entries.push((name.clone(), oid.clone(), _type.to_string()));
                }
            };
        }

        let mut tree_data = String::new();
        for (name, oid, type_) in entries {
            tree_data.push_str(&format!("{type_} {oid} {name}\n"));
        }

        let tree_oid = hash_object(tree_data.as_bytes(), Some("tree"))?;
        Ok(tree_oid)
    }

    write_tree_recursive(&index_as_tree)
}

fn is_ignored(path: &str) -> bool {
    path.split('/').any(|segment| {
        segment == RUGIT_DIR || segment == "target" || segment == ".git"
    })
}

fn iter_tree_entries(oid: &str) -> Result<Vec<(String, String, String)>, Box<dyn Error>> {
    let tree = get_object(oid, Some("tree"))?;
    let tree = str::from_utf8(&tree).unwrap();
    let tree_entries: Vec<(String, String, String)> = tree
        .lines()
        .filter_map(|line| {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() == 3 {
                // Expect exactly 3 parts: (type, object_id, file_name)
                Some((parts[0].to_string(), parts[1].to_string(), parts[2].to_string()))
            } else {
                None
            }
        })
        .collect();
    Ok(tree_entries)
}

pub fn get_tree(oid: &str, base_path: Option<&str>) -> Result<HashMap<String, String>, Box<dyn Error>> {
    let base_path = base_path.unwrap_or("");
    let mut result = HashMap::new();
    let tree_entries = iter_tree_entries(oid)?;
    for (_type, oid, file_name) in tree_entries {
        let path = base_path.to_owned() + &file_name;
        if _type == "blob" {
            result.insert(path, oid);
        } else if _type == "tree" {
            let other_map = get_tree(&oid, Some(&format!("{path}/")))?;
            result.extend(other_map);
        }
    }
    Ok(result)
}

pub fn read_tree(tree_oid: &str, update_working: Option<bool>) -> Result<String, Box<dyn Error>> {
    let update_working = update_working.unwrap_or(false);
    let mut guard = IndexGuard::new()?;
    let index = guard.get_index();
    index.clear();
    index.extend(get_tree(tree_oid, None)?);

    if update_working {
        checkout_index(index)?;
    }

    Ok(String::from("Success"))
}

fn empty_current_directory() -> Result<(), Box<dyn Error>> {
    // Use WalkDir for recursive traversal.
    for entry in walkdir::WalkDir::new(".").sort_by_file_name().into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        if is_ignored(path.to_str().unwrap()) {
            continue;
        }

        if path.is_file() {
            // Remove file
            if let Err(err) = std::fs::remove_file(path) {
                eprintln!("Failed to remove file {:?}: {}", path, err);
            }
        } else if path.is_dir() {
            // Try to remove directory
            if std::fs::remove_dir(path).is_err() {
                // Directory may contain ignored files, so continue
                continue;
            }
        }
    }
    Ok(())
}

pub fn commit(message: &str) -> Result<String, Box<dyn Error>> {
    let mut commit = format!("tree {}\n", write_tree()?);

    let head = match get_ref("HEAD", None) {
            Ok(ref_value) => ref_value.value,
            Err(_) => String::new()
    };

    let parent_oid = head;
    if !parent_oid.is_empty() {
        commit.push_str(&format!("parent {}\n", parent_oid));
    }

    let merge_head = match get_ref("MERGE_HEAD", None) {
            Ok(ref_value) => ref_value.value,
            Err(_) => String::new()
    };
    if !merge_head.is_empty() {
        commit.push_str(&format!("parent {}\n", merge_head));
        delete_ref("MERGE_HEAD", Some(false))?;
    }

    commit.push_str(&format!("\n{message}\n"));

    let oid = hash_object(commit.as_bytes(), Some("commit"))?;
    println!("{oid}");

    update_ref("HEAD", RefValue { symbolic: false, value: oid.clone() }, None, None)?;

    Ok(oid)
}

fn update_ref(_ref: &str, ref_value: RefValue, deref: Option<bool>, is_checkout: Option<bool>) -> Result<(), Box<dyn Error>> {
    let deref = deref.unwrap_or(true);
    let is_checkout = is_checkout.unwrap_or(false);
    let resolved_ref = if is_checkout {
        "HEAD".to_string()
    } else {
        get_ref_internal(_ref, Some(deref))?.0
    };

    assert!(!ref_value.value.is_empty(), "Ref value is empty");
    let mut value = String::new();
    if ref_value.symbolic {
        value = format!("ref: {}", ref_value.value);
    } else {
        value = ref_value.value;
    }

    let ref_path = format!("{RUGIT_DIR}/{resolved_ref}");
    if let Some(parent) = std::path::Path::new(&ref_path).parent() {
        std::fs::create_dir_all(parent)?;
    }
    let mut file = std::fs::File::create(ref_path)?;
    file.write_all(value.as_bytes())?;
    Ok(())
}

fn get_ref(_ref: &str, deref: Option<bool>) -> Result<RefValue, Box<dyn Error>> {
    let deref = deref.unwrap_or(true);
    Ok(get_ref_internal(_ref, Some(deref))?.1)
}

fn get_ref_internal(_ref: &str, deref: Option<bool>) -> Result<(String, RefValue), Box<dyn Error>> {
    let deref = deref.unwrap_or(true);
    let ref_path = format!("{RUGIT_DIR}/{_ref}");
    let path = std::path::Path::new(&ref_path);
    let mut value = String::new();
    let mut symbolic = false;
    if path.is_file() {
        let bytes = std::fs::read(ref_path)?;
        value = String::from_utf8(bytes)?;

        symbolic = !value.is_empty() && value.starts_with("ref:");
        if symbolic {
            let mut split_iter = value.split(":");
            split_iter.next();
            let target_ref = split_iter.next().unwrap().trim();
            if deref {
                return get_ref_internal(target_ref, Some(true))
            }
        }
    };
    Ok((_ref.to_string(), RefValue { symbolic, value }))
}

pub fn get_commit(oid: &str) -> Result<Commit, Box<dyn Error>> {
    let commit = get_object(oid, Some("commit"))?;
    let commit_str = String::from_utf8(commit)?;
    let mut lines = commit_str.lines();

    let mut tree = String::new();
    let mut parents = Vec::new();

    for line in &mut lines {
        if line.is_empty() {
            break;
        }

        let mut split = line.splitn(2, ' ');
        let key = split.next().ok_or("Missing key")?;
        let value = split.next().ok_or("Missing value")?;

        match key {
            "tree" => tree = value.to_string(),
            "parent" => parents.push(value.to_string()),
            _ => return Err(format!("Unknown field: {}", key).into()),
        }
    }

    let message: String = lines.collect::<Vec<&str>>().join("\n");

    Ok(Commit { tree, parents, message })
}

fn iter_commits_and_parents(oid: &str) -> Result<HashSet<String>, Box<dyn Error>> {
    let mut oids = VecDeque::new();
    oids.push_front(oid.to_string());
    let mut visited = HashSet::new();

    while let Some(oid) = oids.pop_front() {
        if oid.is_empty() || visited.contains(&oid) {
            continue;
        }
        visited.insert(oid.clone());
        let commit = get_commit(&oid)?;
        let first_parent = &commit.parents[0];
        oids.push_front(first_parent.to_string());

        if commit.parents.len() > 1 {
            for oid in &commit.parents[1..] {
                oids.push_back(oid.to_string());
            }
        }
    }
    Ok(visited)
}

pub fn log(oid: Option<&str>) -> Result<String, Box<dyn Error>> {
    let oid = match oid {
        Some(oid) => oid.to_string(),
        None => get_ref("HEAD", None)?.value,
    };

    let mut refs: HashMap<String, Vec<String>> = HashMap::new();
    let itered_refs = iter_refs("", None)?;
    for (refname, _ref) in itered_refs {
        refs.entry(_ref.value.clone())
            .or_default()
            .push(refname);
    }

    let commits_and_parents = iter_commits_and_parents(&oid)?;
    for oid in commits_and_parents {
        let commit = get_commit(&oid)?;
        print_commit(&oid, commit, refs.get(&oid).cloned())?;
    }

    Ok(String::from("Success"))
}

pub fn checkout(name: &str) -> Result<String, Box<dyn Error>> {
    let oid = get_oid(name);
    let commit = get_commit(&oid)?;
    read_tree(&commit.tree, Some(true))?;
    let mut head = RefValue { symbolic: false, value: oid.clone() };

    if is_branch(name)? {
        head = RefValue { symbolic: true, value: format!("refs/heads/{name}") };
    }

    update_ref("HEAD", head, Some(false), Some(true))?;
    println!("Switched HEAD to {name}");
    Ok(String::from("Success"))
}

fn is_branch(branch: &str) -> Result<bool, Box<dyn Error>> {
    Ok(!(get_ref(&format!("refs/heads/{branch}"), None)?.value.is_empty()))
}

fn create_tag(name: &str, oid: &str) -> Result<(), Box<dyn Error>> {
    update_ref(&format!("refs/tags/{name}"), RefValue { symbolic: false, value: oid.to_string() }, None, None)?;
    Ok(())
}

pub fn tag(name: &str, oid: Option<&str>) -> Result<String, Box<dyn Error>> {
    let oid = match oid {
        Some(oid) => oid.to_string(),
        None => get_ref("HEAD", None)?.value,
    };
    create_tag(name, &oid)?;
    Ok(String::from("Success"))
}

pub fn get_oid(name: &str) -> String {
    let refs_to_try = [name.to_string(), format!("refs/{name}"), format!("refs/tags/{name}"), format!("refs/heads/{name}")];

    for reference in refs_to_try.iter() {
        let value = get_ref(reference, Some(false)).unwrap().value;
        if !value.is_empty() {
            return get_ref(reference, None).unwrap().value;
        }
    }

    if is_valid_sha1(name) {
        name.to_string()
    } else {
        panic!("Unknown name: {}", name)
    }
}

fn is_valid_sha1(name: &str) -> bool {
    name.len() == 40 && name.chars().all(|c| c.is_ascii_hexdigit())
}

fn iter_refs(prefix: &str, deref: Option<bool>) -> Result<Vec<(String, RefValue)>, Box<dyn Error>> {
    let deref = deref.unwrap_or(true);
    let mut refs = Vec::from([String::from("HEAD"), String::from("MERGE_HEAD")]);
    let refs_dir = format!("{RUGIT_DIR}/refs");

    for entry in walkdir::WalkDir::new(&refs_dir) {
        let entry = entry?;
        if entry.file_type().is_file() {
            let rel_path = entry.path().strip_prefix(RUGIT_DIR)?.to_string_lossy();
            refs.push(rel_path.into_owned());
        }
    }

    let mut ref_results = Vec::new();
    for refname in refs {
        if !refname.starts_with(prefix) {
            continue;
        }
        match get_ref(&refname, Some(deref)) {
            Ok(_ref) => {
                if !_ref.value.is_empty() {
                    ref_results.push((refname, _ref));
                }
            }
            Err(_) => continue,
        }
    }

    Ok(ref_results)
}

pub fn iter_branch_names() -> Result<Vec<String>, Box<dyn Error>> {
    let refs = iter_refs("refs/heads/", None)?;
    let branch_names = refs
        .iter()
        .filter_map(|r| r.0.strip_prefix("refs/heads/").map(|s| s.to_string()))
        .collect();
    Ok(branch_names)
}

pub fn create_branch(name: &str, oid: Option<&str>) -> Result<String, Box<dyn Error>> {
    let oid = match oid {
        Some(oid) => oid.to_string(),
        None => get_ref("HEAD", Some(true))?.value,
    };
    update_ref(&format!("refs/heads/{name}"), RefValue { symbolic: false, value: oid.to_string() }, None, None)?;
    let short_oid = &oid[0..10];
    println!("Branch {name} created at {short_oid}");
    Ok(String::from("Success"))
}

fn get_branch_name() -> Result<String, Box<dyn Error>> {
    let head = get_ref("HEAD", Some(false))?;
    if !head.symbolic {
        return Err(String::from("Head is not symbolic").into())
    }
    let value = head.value;
    assert!(value.starts_with("ref: refs/heads/"), "Invalid branch reference");
    
    let branch_name = Path::new(&value)
        .strip_prefix("ref: refs/heads/")?
        .to_str()
        .ok_or("Failed to convert branch name to string")?
        .to_string();

    Ok(branch_name)
}

pub fn status() -> Result<String, Box<dyn Error>> {
    let head = get_oid("HEAD");
    let branch = get_branch_name()?;
    let short_oid = &head[0..10];
    if !branch.is_empty() {
        println!("On branch {branch}");
    } else {
        println!("HEAD detached at {short_oid}");
    }

    let merge_head = match get_ref("MERGE_HEAD", None) {
            Ok(ref_value) => ref_value.value,
            Err(_) => String::new()
    };

    if !merge_head.is_empty() {
        let short_oid = &merge_head[0..10];
        println!("Merging with {short_oid}");
    }

    println!("\nChanges to be committed:\n");
    let head_tree = get_commit(&head)?.tree;
    let mut guard = IndexGuard::new()?;
    let index_tree = guard.get_index();
    for (path, action) in iter_changed_files(get_tree(&head_tree, None)?, index_tree.clone()) {
        println!("{action}: {path}");
    }

    println!("\nChanges not staged for commit:\n");
    for (path, action) in iter_changed_files(index_tree.clone(), get_working_tree()?) {
        println!("{action}: {path}");
    }

    Ok(String::from("Success"))
}

pub fn print_branch_names() -> Result<String, Box<dyn Error>> {
    let current = get_branch_name()?;
    let branch_names = iter_branch_names()?;
    for branch in branch_names {
        let mut prefix = " ";
        if branch == current {
            prefix = "*";
        }
        println!("{prefix} {branch}");
    }
    Ok(String::from("Success"))
}

pub fn reset(oid: &str) -> Result<String, Box<dyn Error>> {
    update_ref("HEAD", RefValue { symbolic: false, value: oid.to_string() }, Some(true), Some(false))?;
    Ok(String::from("Success"))
}

pub fn print_commit(oid: &str, commit: Commit, refs: Option<Vec<String>>) -> Result<String, Box<dyn Error>> {
    let refs = refs.unwrap_or_default();
    let mut refs_str = String::from("");
    if !refs.join(", ").is_empty() {
        refs_str = format!("({})", refs.join(", "));
    }
    println!("commit {oid} {refs_str}");
    println!("\t{}\t\n", commit.message);
    Ok(String::from("Success"))
}

fn compare_trees(trees: Vec<HashMap<String, String>>) -> Vec<(String, Vec<String>)> {
    let mut entries: HashMap<String, Vec<String>> = HashMap::new();
    let mut result: Vec<(String, Vec<String>)> = Vec::new();

    for (i, tree) in trees.iter().enumerate() {
        for (path, oid) in tree.iter() {
            entries.entry(path.to_string())
                .or_insert_with(|| vec![String::new(); trees.len()])
                [i] = oid.to_string();
        }
    }

    for (path, oids) in entries.iter() {
        result.push((path.to_string(), oids.to_vec()));
    }

    result
}

pub fn diff_trees(t_from: HashMap<String, String>, t_to: HashMap<String, String>) -> Result<String, Box<dyn Error>> {
    let mut output = String::new();
    for (path, oids) in compare_trees(vec![t_from, t_to]) {
        let o_from = oids[0].clone();
        let o_to = oids[1].clone();

        if o_from != o_to {
            output += &format!("changed: {path}\n");
            output += &diff_blobs(&o_from, &o_to)?;
            output += "\n";
        }
    }

    Ok(output)
}

// TODO: Add colors and line numbers and only show nearby lines.
fn diff_blobs(o_from: &str, o_to: &str) -> Result<String, Box<dyn Error>> {
    // TODO: Fix these to show actual line additions or deletions instead of just created / deleted.
    if o_from.is_empty() {
        return Ok(String::from("Created file\n"));
    }
    if o_to.is_empty() {
        return Ok(String::from("Deleted file\n"))
    }
    let from_text = String::from_utf8(get_object(o_from, Some("blob"))?)?;
    let to_text = String::from_utf8(get_object(o_to, Some("blob"))?)?;

    let diff = TextDiff::from_lines(&from_text, &to_text);
    let mut output = String::new();

    for change in diff.iter_all_changes() {
        let sign = match change.tag() {
            ChangeTag::Delete => "-",
            ChangeTag::Insert => "+",
            ChangeTag::Equal => " ",
        };
        output += &format!("{}{}", sign, change);
    }
    Ok(output)
}

pub fn get_working_tree() -> Result<HashMap<String, String>, Box<dyn Error>> {
    let mut result = HashMap::new();
    let current_dir = std::env::current_dir()?.canonicalize()?;
    for entry in walkdir::WalkDir::new(".").sort_by_file_name().into_iter().filter_map(|e| e.ok()) {
        let path = entry.path().canonicalize()?;

        if is_ignored(path.to_str().unwrap_or("")) || !path.is_file() {
            continue;
        }

        let rel_path = path.strip_prefix(&current_dir)?;

        let contents = std::fs::read(&path)?;
        let hashed_contents = hash_object(&contents, None)?;

        result.insert(rel_path.to_str().unwrap().to_string(), hashed_contents);
    }

    Ok(result)
}

fn iter_changed_files(t_from: HashMap<String, String>, t_to: HashMap<String, String>) -> Vec<(String, String)> {
    let mut result: Vec<(String, String)> = Vec::new();
    for (path, oids) in compare_trees(vec![t_from, t_to]) {
        let o_from = oids[0].clone();
        let o_to = oids[1].clone();

        if o_from != o_to {
            let action = if o_from.is_empty() {
                "new file".to_string()
            } else if o_to.is_empty() {
                "deleted".to_string()
            } else {
                "modified".to_string()
            };
            result.push((path, action))
        }
    }

    result
}

fn merge_trees(t_base: HashMap<String, String>, t_head: HashMap<String, String>, t_other: HashMap<String, String>) -> Result<HashMap<String, String>, Box <dyn Error>> {
    let mut tree = HashMap::new();
    for (path, object_oids) in compare_trees(vec![t_base, t_head, t_other]) {
        let o_base = object_oids[0].clone();
        let o_head = object_oids[1].clone();
        let o_other = object_oids[2].clone();

        tree.insert(path.clone(), hash_object(&merge_blobs(&o_base, &o_head, &o_other)?, None)?);
    }

    Ok(tree)
}

fn merge_blobs(o_base: &str, o_head: &str, o_other: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut temp_base = NamedTempFile::new()?;
    let mut temp_head = NamedTempFile::new()?;
    let mut temp_other = NamedTempFile::new()?;

    let base_data = get_object(o_base, None)?;
    temp_base.write_all(&base_data)?;
    temp_base.flush()?;

    let head_data = get_object(o_head, None)?;
    temp_head.write_all(&head_data)?;
    temp_head.flush()?;

    let other_data = get_object(o_other, None)?;
    temp_other.write_all(&other_data)?;
    temp_other.flush()?;

    let output = Command::new("diff3")
        .arg("-m")
        .arg("-L")
        .arg("HEAD")
        .arg(temp_head.path())
        .arg("-L")
        .arg("BASE")
        .arg(temp_base.path())
        .arg("-L")
        .arg("MERGE_HEAD")
        .arg(temp_other.path())
        .output()?;

    match output.status.code() {
        Some(0) | Some(1) => Ok(output.stdout),
        _ => panic!("diff3 command failed with status: {:?}", output.status)
    }
}

fn read_tree_merged(t_base: &str, t_head: &str, t_other: &str, update_working: Option<bool>) -> Result<(), Box<dyn Error>> {
    let update_working = update_working.unwrap_or(false);
    let mut guard = IndexGuard::new()?;
    let index = guard.get_index();
    index.clear();
    index.extend(merge_trees(get_tree(t_base, None)?, get_tree(t_head, None)?, get_tree(t_other, None)?)?);

    if update_working {
        checkout_index(index)?;
    }
    Ok(())
}

fn checkout_index(index: &HashMap<String, String>) -> Result<(), Box<dyn Error>> {
    empty_current_directory()?;
    for (path, oid) in index {
        if let Some(parent) = Path::new(&path).parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut file = std::fs::File::create(path)?;
        file.write_all(&get_object(oid, Some("blob"))?)?;
    }

    Ok(())
}

pub fn merge(commit_oid: &str) -> Result<String, Box<dyn Error>> {
    let head = get_ref("HEAD", None)?.value;
    let merge_base = get_merge_base(commit_oid, &head)?;

    let commit_other = get_commit(commit_oid)?;

    if merge_base == head {
        read_tree(&commit_other.tree, Some(true))?;
        update_ref("HEAD", RefValue { symbolic: false, value: commit_oid.to_string() }, None, None)?;
        println!("Fast-forward merge, no need to commit");
        return Ok(String::from("Success"));
    }

    update_ref("MERGE_HEAD", RefValue { symbolic: false, value: commit_oid.to_string() }, None, None)?;

    let commit_base = get_commit(&merge_base)?;
    let commit_head = get_commit(&head)?;

    read_tree_merged(&commit_base.tree, &commit_head.tree, &commit_other.tree, Some(true))?;
    println!("Merged in working tree\nPlease commit");
    Ok(String::from("Success"))
}

fn delete_ref(_ref: &str, deref: Option<bool>) -> Result<(), Box<dyn Error>> {
    let deref = deref.unwrap_or(true);
    let _ref = get_ref_internal(_ref, Some(deref))?.0;
    std::fs::remove_file(format!("{RUGIT_DIR}/{_ref}"))?;
    Ok(())
}

pub fn get_merge_base(oid1: &str, oid2: &str) -> Result<String, Box<dyn Error>> {
    let parents1 = iter_commits_and_parents(oid1)?;
    let parents2 = iter_commits_and_parents(oid2)?;

    for oid in parents2 {
        if parents1.contains(&oid) {
            return Ok(oid)
        }
    }

    Ok(String::from(""))
}

fn normalize_path(path: &str) -> Result<String, Box<dyn Error>> {
    let current_dir = std::env::current_dir()?;
    let abs_path = Path::new(path).canonicalize()?;
    let rel_path = abs_path.strip_prefix(&current_dir)?;
    Ok(rel_path.to_string_lossy().into_owned())
}

pub fn add(filenames: Vec<String>) -> Result<String, Box<dyn Error>> {
    fn add_file(filename: String, guard: &mut IndexGuard) -> Result<(), Box<dyn Error>> {
        let index = guard.get_index();

        let normalized_path = normalize_path(&filename)?;

        let content = std::fs::read(normalized_path.clone())?;
        let oid = hash_object(&content, None)?;
        index.insert(normalized_path, oid);
        Ok(())
    }

    fn add_directory(dirname: String, guard: &mut IndexGuard) -> Result<(), Box<dyn Error>> {
        for entry in walkdir::WalkDir::new(dirname).sort_by_file_name().into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            let normalized_path = normalize_path(path.to_str().unwrap())?;
            if is_ignored(path.to_str().unwrap()) || !path.is_file() {
                continue;
            }
            add_file(normalized_path, guard)?;
        }
        Ok(())
    }

    let mut guard = IndexGuard::new()?;
    
    for name in filenames {
        let path = std::path::Path::new(&name);
        if path.is_file() {
            add_file(name, &mut guard)?;
        } else if path.is_dir() {
            add_directory(name, &mut guard)?;
        }
    }

    Ok(String::from("Success"))
}
