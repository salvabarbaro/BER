#!/bin/bash

cd ~/Documents/Research/Yardstick/BER/github/BER/ || { echo "Directory not found."; exit 1; }

# Print current branch
BRANCH=$(git symbolic-ref --short HEAD)
echo "Syncing branch: $BRANCH"

# Pull latest changes from origin
echo "Pulling from GitHub..."
git pull origin "$BRANCH"

# Stage all changes (modified, new, deleted)
echo "Adding all changes..."
git add -A

# Commit with automatic message including timestamp
COMMIT_MSG="Auto-sync on $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$COMMIT_MSG"

# Push to GitHub
echo "Pushing to GitHub..."
git push origin "$BRANCH"

echo "Sync complete."

