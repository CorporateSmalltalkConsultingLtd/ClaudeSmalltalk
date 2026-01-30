#!/usr/bin/env python3
"""
Smalltalk Project Manager for Clawdbot
JMM-512: Interactive mode selection (Playground vs Development)

Manages:
- Mode selection (playground/new project/load existing)
- Recent projects tracking
- Snapshots for safety
"""

import os
import sys
import json
import shutil
from datetime import datetime
from pathlib import Path

# Configuration
CONFIG_DIR = Path.home() / '.clawdbot' / 'smalltalk'
RECENT_FILE = CONFIG_DIR / 'recent-projects.json'
PROJECTS_DIR = Path.home() / 'smalltalk-projects'

def ensure_dirs():
    """Create config and projects directories if needed."""
    CONFIG_DIR.mkdir(parents=True, exist_ok=True)
    PROJECTS_DIR.mkdir(parents=True, exist_ok=True)

def load_recent():
    """Load recent projects list."""
    if RECENT_FILE.exists():
        try:
            return json.loads(RECENT_FILE.read_text())
        except:
            return []
    return []

def save_recent(recent):
    """Save recent projects list."""
    ensure_dirs()
    RECENT_FILE.write_text(json.dumps(recent, indent=2))

def add_to_recent(project_path, project_name):
    """Add a project to recent list."""
    recent = load_recent()
    entry = {
        'name': project_name,
        'path': str(project_path),
        'last_used': datetime.now().isoformat()
    }
    # Remove if already exists
    recent = [r for r in recent if r['path'] != str(project_path)]
    # Add to front
    recent.insert(0, entry)
    # Keep only last 10
    recent = recent[:10]
    save_recent(recent)

def list_recent():
    """List recent projects."""
    recent = load_recent()
    if not recent:
        print("No recent projects.")
        return []
    print("\n📂 Recent Projects:")
    for i, proj in enumerate(recent, 1):
        last = proj.get('last_used', 'unknown')
        if last != 'unknown':
            try:
                dt = datetime.fromisoformat(last)
                last = dt.strftime('%Y-%m-%d %H:%M')
            except:
                pass
        exists = "✅" if Path(proj['path']).exists() else "❌"
        print(f"   {i}. {exists} {proj['name']} ({last})")
        print(f"      {proj['path']}")
    return recent

def create_project(name, base_image, base_changes):
    """Create a new project by copying image and changes."""
    ensure_dirs()
    project_dir = PROJECTS_DIR / name
    
    if project_dir.exists():
        print(f"❌ Project '{name}' already exists at {project_dir}")
        return None
    
    # Verify base files exist
    if not Path(base_image).exists():
        print(f"❌ Base image not found: {base_image}")
        return None
    if not Path(base_changes).exists():
        print(f"❌ Base changes file not found: {base_changes}")
        return None
    
    # Create project structure
    project_dir.mkdir(parents=True)
    snapshots_dir = project_dir / 'snapshots'
    snapshots_dir.mkdir()
    
    # Copy files
    image_dest = project_dir / f'{name}.image'
    changes_dest = project_dir / f'{name}.changes'
    
    print(f"📁 Creating project: {project_dir}")
    print(f"   Copying image...")
    shutil.copy2(base_image, image_dest)
    print(f"   Copying changes...")
    shutil.copy2(base_changes, changes_dest)
    
    # Create project metadata
    meta = {
        'name': name,
        'created': datetime.now().isoformat(),
        'base_image': str(base_image),
        'image': str(image_dest),
        'changes': str(changes_dest)
    }
    (project_dir / 'project.json').write_text(json.dumps(meta, indent=2))
    
    add_to_recent(project_dir, name)
    
    print(f"✅ Project created: {name}")
    print(f"   Image: {image_dest}")
    print(f"   Changes: {changes_dest}")
    
    return {
        'dir': project_dir,
        'image': image_dest,
        'changes': changes_dest,
        'name': name
    }

def load_project(path):
    """Load an existing project."""
    project_dir = Path(path)
    meta_file = project_dir / 'project.json'
    
    if meta_file.exists():
        meta = json.loads(meta_file.read_text())
        image = Path(meta['image'])
        changes = Path(meta['changes'])
    else:
        # Try to find image/changes in directory
        images = list(project_dir.glob('*.image'))
        if not images:
            print(f"❌ No .image file found in {project_dir}")
            return None
        image = images[0]
        changes = image.with_suffix('.changes')
        if not changes.exists():
            print(f"❌ Changes file not found: {changes}")
            return None
        meta = {'name': image.stem}
    
    if not image.exists():
        print(f"❌ Image not found: {image}")
        return None
    
    add_to_recent(project_dir, meta.get('name', project_dir.name))
    
    print(f"✅ Loaded project: {meta.get('name', project_dir.name)}")
    print(f"   Image: {image}")
    print(f"   Changes: {changes}")
    
    return {
        'dir': project_dir,
        'image': image,
        'changes': changes,
        'name': meta.get('name', project_dir.name)
    }

def create_snapshot(project_dir, name=None):
    """Create a snapshot of current project state."""
    project_dir = Path(project_dir)
    snapshots_dir = project_dir / 'snapshots'
    snapshots_dir.mkdir(exist_ok=True)
    
    timestamp = datetime.now().strftime('%Y%m%d-%H%M%S')
    snap_name = name or f'snapshot-{timestamp}'
    snap_dir = snapshots_dir / snap_name
    snap_dir.mkdir()
    
    # Find and copy image/changes
    for img in project_dir.glob('*.image'):
        if 'snapshot' not in str(img):
            shutil.copy2(img, snap_dir / img.name)
            changes = img.with_suffix('.changes')
            if changes.exists():
                shutil.copy2(changes, snap_dir / changes.name)
            break
    
    print(f"✅ Snapshot created: {snap_name}")
    return snap_dir

def list_snapshots(project_dir):
    """List available snapshots."""
    project_dir = Path(project_dir)
    snapshots_dir = project_dir / 'snapshots'
    
    if not snapshots_dir.exists():
        print("No snapshots.")
        return []
    
    snaps = sorted(snapshots_dir.iterdir(), key=lambda p: p.stat().st_mtime, reverse=True)
    if not snaps:
        print("No snapshots.")
        return []
    
    print("\n📸 Snapshots:")
    for i, snap in enumerate(snaps, 1):
        mtime = datetime.fromtimestamp(snap.stat().st_mtime).strftime('%Y-%m-%d %H:%M')
        print(f"   {i}. {snap.name} ({mtime})")
    return snaps

def restore_snapshot(project_dir, snapshot_name):
    """Restore a snapshot."""
    project_dir = Path(project_dir)
    snap_dir = project_dir / 'snapshots' / snapshot_name
    
    if not snap_dir.exists():
        print(f"❌ Snapshot not found: {snapshot_name}")
        return False
    
    # Create backup of current state first
    create_snapshot(project_dir, f'pre-restore-{datetime.now().strftime("%Y%m%d-%H%M%S")}')
    
    # Restore files
    for f in snap_dir.glob('*'):
        dest = project_dir / f.name
        print(f"   Restoring {f.name}...")
        shutil.copy2(f, dest)
    
    print(f"✅ Restored snapshot: {snapshot_name}")
    return True

def interactive_mode_select(default_image, default_changes):
    """Interactive mode selection menu."""
    print("\n" + "="*60)
    print("🎯 Smalltalk Mode Selection")
    print("="*60)
    print()
    print("  1. Playground     - Ephemeral sandbox (no persistence)")
    print("  2. New Project    - Create saveable working copy")
    print("  3. Load Project   - Open existing project")
    
    recent = load_recent()
    recent_valid = [r for r in recent if Path(r['path']).exists()]
    
    if recent_valid:
        print()
        print("  Recent:")
        for i, proj in enumerate(recent_valid[:3], 4):
            print(f"  {i}. {proj['name']}")
    
    print()
    choice = input("Select [1]: ").strip() or '1'
    
    if choice == '1':
        return {'mode': 'playground'}
    
    elif choice == '2':
        print()
        name = input("Project name: ").strip()
        if not name:
            print("❌ Name required")
            return None
        proj = create_project(name, default_image, default_changes)
        if proj:
            return {'mode': 'development', 'project': proj}
        return None
    
    elif choice == '3':
        print()
        path = input("Project path or image file: ").strip()
        if not path:
            print("❌ Path required")
            return None
        path = Path(path).expanduser()
        if path.suffix == '.image':
            path = path.parent
        proj = load_project(path)
        if proj:
            return {'mode': 'development', 'project': proj}
        return None
    
    elif choice.isdigit() and int(choice) >= 4:
        idx = int(choice) - 4
        if idx < len(recent_valid):
            proj = load_project(recent_valid[idx]['path'])
            if proj:
                return {'mode': 'development', 'project': proj}
        print("❌ Invalid selection")
        return None
    
    else:
        print("❌ Invalid selection")
        return None

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Smalltalk Project Manager')
    parser.add_argument('command', choices=['select', 'recent', 'create', 'load', 'snapshot', 'snapshots', 'restore'])
    parser.add_argument('--name', help='Project or snapshot name')
    parser.add_argument('--path', help='Project path')
    parser.add_argument('--image', default='/home/johnmci/ClaudeSqueak.image', help='Base image')
    parser.add_argument('--changes', default='/home/johnmci/ClaudeSqueak.changes', help='Base changes')
    
    args = parser.parse_args()
    
    if args.command == 'select':
        result = interactive_mode_select(args.image, args.changes)
        if result:
            print(json.dumps(result))
    elif args.command == 'recent':
        list_recent()
    elif args.command == 'create':
        if not args.name:
            print("❌ --name required")
            sys.exit(1)
        create_project(args.name, args.image, args.changes)
    elif args.command == 'load':
        if not args.path:
            print("❌ --path required")
            sys.exit(1)
        load_project(args.path)
    elif args.command == 'snapshot':
        if not args.path:
            print("❌ --path required")
            sys.exit(1)
        create_snapshot(args.path, args.name)
    elif args.command == 'snapshots':
        if not args.path:
            print("❌ --path required")
            sys.exit(1)
        list_snapshots(args.path)
    elif args.command == 'restore':
        if not args.path or not args.name:
            print("❌ --path and --name required")
            sys.exit(1)
        restore_snapshot(args.path, args.name)
