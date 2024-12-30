# rugit

rugit is a git implementation written in Rust. This project recreates core git functionality to help understand git's internal workings.

## Installation

```bash
# Install from crates.io
cargo install rugit

# Or build from source
git clone https://github.com/yourusername/rugit.git
cd rugit
cargo install --path .
```
## Commands

### Basic
```bash
# Initialize a new repository
rugit init

# Check status of working directory
rugit status

# Add files to commit
rugit add <file>

# Create a new commit
rugit commit -m "Your commit message"

# View commit history
rugit log

# Create a new branch
rugit branch branch-name

# Switch to a different commit/branch
rugit checkout <commit-hash or branch-name>
```

### Advanced
```bash
# Create a tag
rugit tag v1.0.0

# View differences
rugit diff
rugit diff --cached

# Merge branches
rugit merge <commit-hash or branch-name>

# Reset to specific commit
rugit reset <commit-hash>

# Show commit details
rugit show <commit-hash>
```

Warning: rugit is currently in an alpha state and not fully stable / has known issues, use at your own risk. (After initializing might need to commit to start tracking files)
