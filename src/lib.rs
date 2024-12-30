use std::error::Error;
use clap::{Parser, Subcommand, Args};
use commands::{get_oid, IndexGuard};

pub mod commands;

pub const RUGIT_DIR: &str = ".rugit";

#[derive(Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Init,
    HashObject {
        file_path: String,
    },
    CatFile {
        object_id: String,
    },
    WriteTree,
    ReadTree {
        object_id: String,
    },
    Commit(CommitArgs),
    Log {
        object_id: Option<String>,
    },
    Checkout {
        commit: String,
    },
    Tag {
        name: String,
        object_id: Option<String>,
    },
    Branch {
        name: Option<String>,
        start_oid: Option<String>
    },
    Status,
    Reset {
        commit: String,
    },
    Show {
        object_id: Option<String>,
    },
    Diff {
        #[arg(short, long)]
        cached: bool,
        commit: Option<String>
    },
    Merge {
        commit: String
    },
    MergeBase {
        commit1: String,
        commit2: String
    },
    Add {
        #[arg(value_parser, num_args = 1.., value_delimiter = ' ')]
        files: Vec<String>
    },
}

#[derive(Args)]
pub struct CommitArgs {
    #[arg(short = 'm', long = "message")]
    message: String,
}

pub fn run(cli: &Cli) -> Result<(), Box<dyn Error>> {
    let _ = match &cli.command {
        Some(Commands::Init) => commands::init(),
        Some(Commands::HashObject{file_path}) => commands::hash_object(&std::fs::read(file_path)?, None),
        Some(Commands::CatFile{object_id}) => commands::cat_file(&get_oid(object_id)),
        Some(Commands::WriteTree) => commands::write_tree(),
        Some(Commands::ReadTree{object_id}) => commands::read_tree(&get_oid(object_id), None),
        Some(Commands::Commit(commit_args)) => commands::commit(&commit_args.message),
        Some(Commands::Log { object_id }) => {
            if let Some(oid) = object_id {
                let oid = get_oid(oid);
                commands::log(Some(oid).as_deref())
            } else {
                commands::log(None)
            }
        }
        Some(Commands::Checkout { commit }) => commands::checkout(commit),
        Some(Commands::Tag { name, object_id }) => {
            if let Some(oid) = object_id {
                let oid = get_oid(oid);
                commands::tag(name, Some(oid).as_deref())
            } else {
                commands::tag(name, None)
            }
        }
        Some(Commands::Branch { name, start_oid }) => {
            if let Some(branch_name) = name {
                let oid = if let Some(start_oid) = start_oid {
                    let oid = get_oid(start_oid);
                    Some(oid)
                } else {
                    None
                };
                commands::create_branch(branch_name, oid.as_deref())
            } else {
                commands::print_branch_names()
            }
        }
        Some(Commands::Status) => commands::status(),
        Some(Commands::Reset { commit }) => {
            let commit = get_oid(commit);
            commands::reset(&commit)
        }
        Some(Commands::Show { object_id }) => {
            // In future can expand show to work on different objects.
            // And can turn this into a show() function defined elsewhere.
            //
            // let type = get_object_type(object_id)?
            
            if let Some(oid) = object_id {
                let oid = get_oid(oid);
                let commit = commands::get_commit(&oid)?;
                let mut parent_tree = String::new();
                if !commit.parents.is_empty() {
                    let parent_oid = commit.parents[0].clone();
                    parent_tree = commands::get_commit(&parent_oid)?.tree;
                }
                let result = commands::diff_trees(
                    commands::get_tree(&parent_tree, None)?,
                    commands::get_tree(&commit.tree, None)?
                )?;
                commands::print_commit(&oid, commit, None)?;
                println!("{result}");
                Ok(String::from("Success"))
            } else {
                let oid = get_oid("HEAD");
                let commit = commands::get_commit(&oid)?;
                let mut parent_tree = String::new();
                if !commit.parents.is_empty() {
                    let parent_oid = commit.parents[0].clone();
                    parent_tree = commands::get_commit(&parent_oid)?.tree;
                }
                let result = commands::diff_trees(
                    commands::get_tree(&parent_tree, None)?,
                    commands::get_tree(&commit.tree, None)?
                )?;
                commands::print_commit(&oid, commit, None)?;
                println!("{result}");
                Ok(String::from("Success"))
            }
        }
        Some(Commands::Diff { cached, commit }) => {
            let tree_from;
            let tree_to;

            if let Some(commit_oid) = commit {
                let oid = get_oid(commit_oid);
                tree_from = commands::get_tree(&commands::get_commit(&oid)?.tree, None)?;
            } else if *cached {
                let oid = get_oid("HEAD");
                tree_from = commands::get_tree(&commands::get_commit(&oid)?.tree, None)?;
            } else {
                let mut guard = IndexGuard::new()?;
                let index = guard.get_index();
                tree_from = index.clone();
            }

            if *cached {
                let mut guard = IndexGuard::new()?;
                let index = guard.get_index();
                tree_to = index.clone();
            } else {
                tree_to = commands::get_working_tree()?;
            }

            let result = commands::diff_trees(tree_from, tree_to)?;
            println!("{result}");
            Ok(String::from("Success"))
        }
        Some(Commands::Merge { commit }) => {
            commands::merge(&get_oid(commit))
        }
        Some(Commands::MergeBase { commit1, commit2 }) => {
            let commit1 = get_oid(commit1);
            let commit2 = get_oid(commit2);

            println!("{}", commands::get_merge_base(&commit1, &commit2)?);
            Ok(String::from("Success"))
        }
        Some(Commands::Add { files }) => commands::add(files.to_vec()),
        None => Ok(String::from("No command")),
    };
    Ok(())
}
