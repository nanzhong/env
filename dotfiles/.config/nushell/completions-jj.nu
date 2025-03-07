module completions {

  # Jujutsu (An experimental VCS)
  export extern jj [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
    --version(-V)             # Print version
  ]

  # Abandon a revision
  export extern "jj abandon" [
    ...revisions_pos: string  # The revision(s) to abandon (default: @)
    -r: string
    --summary(-s)             # Do not print every abandoned commit on a separate line
    --restore-descendants     # Do not modify the content of the children of the abandoned commits
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Move changes from a revision into the stack of mutable revisions
  export extern "jj absorb" [
    --from(-f): string        # Source revision to absorb from
    --into(-t): string        # Destination revisions to absorb into
    --to: string              # Destination revisions to absorb into
    ...paths: string          # Move only changes to these paths (instead of all paths)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Apply the reverse of a revision on top of another revision
  export extern "jj backout" [
    --revisions(-r): string   # The revision(s) to apply the reverse of
    --destination(-d): string # The revision to apply the reverse changes on top of
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Manage bookmarks [default alias: b]
  export extern "jj bookmark" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create a new bookmark
  export extern "jj bookmark create" [
    --revision(-r): string    # The bookmark's target revision
    --to: string              # The bookmark's target revision
    ...names: string          # The bookmarks to create
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Delete an existing bookmark and propagate the deletion to remotes on the next push
  export extern "jj bookmark delete" [
    ...names: string          # The bookmarks to delete
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Forget everything about a bookmark, including its local and remote targets
  export extern "jj bookmark forget" [
    ...names: string          # The bookmarks to forget
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List bookmarks and their targets
  export extern "jj bookmark list" [
    --all-remotes(-a)         # Show all tracking and non-tracking remote bookmarks including the ones whose targets are synchronized with the local bookmarks
    --remote: string          # Show all tracking and non-tracking remote bookmarks belonging to this remote
    --tracked(-t)             # Show remote tracked bookmarks only. Omits local Git-tracking bookmarks by default
    --conflicted(-c)          # Show conflicted bookmarks only
    ...names: string          # Show bookmarks whose local name matches
    --revisions(-r): string   # Show bookmarks whose local targets are in the given revisions
    --template(-T): string    # Render each bookmark using the given template
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Move existing bookmarks to target revision
  export extern "jj bookmark move" [
    --from: string            # Move bookmarks from the given revisions
    --to: string              # Move bookmarks to this revision
    --allow-backwards(-B)     # Allow moving bookmarks backwards or sideways
    ...names: string          # Move bookmarks matching the given name patterns
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Rename `old` bookmark name to `new` bookmark name
  export extern "jj bookmark rename" [
    old: string               # The old name of the bookmark
    new: string               # The new name of the bookmark
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create or update a bookmark to point to a certain commit
  export extern "jj bookmark set" [
    --revision(-r): string    # The bookmark's target revision
    --to: string              # The bookmark's target revision
    --allow-backwards(-B)     # Allow moving the bookmark backwards or sideways
    ...names: string          # The bookmarks to update
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Start tracking given remote bookmarks
  export extern "jj bookmark track" [
    ...names: string          # Remote bookmarks to track
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Stop tracking given remote bookmarks
  export extern "jj bookmark untrack" [
    ...names: string          # Remote bookmarks to untrack
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Manage bookmarks [default alias: b]
  export extern "jj branch" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create a new bookmark
  export extern "jj branch create" [
    --revision(-r): string    # The bookmark's target revision
    --to: string              # The bookmark's target revision
    ...names: string          # The bookmarks to create
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Delete an existing bookmark and propagate the deletion to remotes on the next push
  export extern "jj branch delete" [
    ...names: string          # The bookmarks to delete
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Forget everything about a bookmark, including its local and remote targets
  export extern "jj branch forget" [
    ...names: string          # The bookmarks to forget
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List bookmarks and their targets
  export extern "jj branch list" [
    --all-remotes(-a)         # Show all tracking and non-tracking remote bookmarks including the ones whose targets are synchronized with the local bookmarks
    --remote: string          # Show all tracking and non-tracking remote bookmarks belonging to this remote
    --tracked(-t)             # Show remote tracked bookmarks only. Omits local Git-tracking bookmarks by default
    --conflicted(-c)          # Show conflicted bookmarks only
    ...names: string          # Show bookmarks whose local name matches
    --revisions(-r): string   # Show bookmarks whose local targets are in the given revisions
    --template(-T): string    # Render each bookmark using the given template
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Move existing bookmarks to target revision
  export extern "jj branch move" [
    --from: string            # Move bookmarks from the given revisions
    --to: string              # Move bookmarks to this revision
    --allow-backwards(-B)     # Allow moving bookmarks backwards or sideways
    ...names: string          # Move bookmarks matching the given name patterns
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Rename `old` bookmark name to `new` bookmark name
  export extern "jj branch rename" [
    old: string               # The old name of the bookmark
    new: string               # The new name of the bookmark
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create or update a bookmark to point to a certain commit
  export extern "jj branch set" [
    --revision(-r): string    # The bookmark's target revision
    --to: string              # The bookmark's target revision
    --allow-backwards(-B)     # Allow moving the bookmark backwards or sideways
    ...names: string          # The bookmarks to update
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Start tracking given remote bookmarks
  export extern "jj branch track" [
    ...names: string          # Remote bookmarks to track
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Stop tracking given remote bookmarks
  export extern "jj branch untrack" [
    ...names: string          # Remote bookmarks to untrack
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Print contents of files in a revision
  export extern "jj cat" [
    --revision(-r): string    # The revision to get the file contents from
    ...paths: string          # Paths to print
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj chmod mode" [] {
    [ "n" "x" ]
  }

  # Sets or removes the executable bit for paths in the repo
  export extern "jj chmod" [
    mode: string@"nu-complete jj chmod mode"
    --revision(-r): string    # The revision to update
    ...paths: string          # Paths to change the executable bit for
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update the description and create a new change on top
  export extern "jj commit" [
    --interactive(-i)         # Interactively choose which changes to include in the first commit
    --tool: string            # Specify diff editor to be used (implies --interactive)
    --message(-m): string     # The change description to use (don't open editor)
    ...paths: string          # Put these paths in the first commit
    --reset-author            # Reset the author to the configured user
    --author: string          # Set author to the provided string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Manage config options
  export extern "jj config" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Start an editor on a jj config file
  export extern "jj config edit" [
    --user                    # Target the user-level config
    --repo                    # Target the repo-level config
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Get the value of a given config option.
  export extern "jj config get" [
    name: string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List variables set in config file, along with their values
  export extern "jj config list" [
    name?: string             # An optional name of a specific config option to look up
    --include-defaults        # Whether to explicitly include built-in default values in the list
    --include-overridden      # Allow printing overridden values
    --user                    # Target the user-level config
    --repo                    # Target the repo-level config
    --template(-T): string    # Render each variable using the given template
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Print the path to the config file
  export extern "jj config path" [
    --user                    # Target the user-level config
    --repo                    # Target the repo-level config
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update config file to set the given option to a given value
  export extern "jj config set" [
    name: string
    value: string             # New value to set
    --user                    # Target the user-level config
    --repo                    # Target the repo-level config
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update config file to unset the given option
  export extern "jj config unset" [
    name: string
    --user                    # Target the user-level config
    --repo                    # Target the repo-level config
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Low-level commands not intended for users
  export extern "jj debug" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Rebuild commit index
  export extern "jj debug copy-detection" [
    revision?: string         # Show changes in this revision, compared to its parent(s)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Parse fileset expression
  export extern "jj debug fileset" [
    path: string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show commit index stats
  export extern "jj debug index" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show information about the local working copy state
  export extern "jj debug local-working-copy" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj debug operation display" [] {
    [ "operation" "id" "view" "all" ]
  }

  # Show information about an operation and its view
  export extern "jj debug operation" [
    operation?: string
    --display: string@"nu-complete jj debug operation display"
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Rebuild commit index
  export extern "jj debug reindex" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Evaluate revset to full commit IDs
  export extern "jj debug revset" [
    revision: string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Trigger a snapshot in the op log
  export extern "jj debug snapshot" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Parse a template
  export extern "jj debug template" [
    template: string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List the recursive entries of a tree
  export extern "jj debug tree" [
    --revision(-r): string
    --id: string
    --dir: string
    ...paths: string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  export extern "jj debug watchman" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Check whether `watchman` is enabled and whether it's correctly installed
  export extern "jj debug watchman status" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  export extern "jj debug watchman query-clock" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  export extern "jj debug watchman query-changed-files" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  export extern "jj debug watchman reset-clock" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show information about the working copy state
  export extern "jj debug working-copy" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update the change description or other metadata
  export extern "jj describe" [
    ...revisions_pos: string  # The revision(s) whose description to edit (default: @)
    -r: string
    --message(-m): string     # The change description to use (don't open editor)
    --stdin                   # Read the change description from stdin
    --no-edit                 # Don't open an editor
    --reset-author            # Reset the author to the configured user
    --author: string          # Set author to the provided string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Compare file contents between two revisions
  export extern "jj diff" [
    --revision(-r): string    # Show changes in this revision, compared to its parent(s)
    --from(-f): string        # Show changes from this revision
    --to(-t): string          # Show changes to this revision
    ...paths: string          # Restrict the diff to these paths
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space(-w)    # Ignore whitespace when comparing lines
    --ignore-space-change(-b) # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Touch up the content changes in a revision with a diff editor
  export extern "jj diffedit" [
    --revision(-r): string    # The revision to touch up
    --from(-f): string        # Show changes from this revision
    --to(-t): string          # Edit changes in this revision
    --tool: string            # Specify diff editor to be used
    --restore-descendants     # Preserve the content (not the diff) when rebasing descendants
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create new changes with the same content as existing ones
  export extern "jj duplicate" [
    ...revisions_pos: string  # The revision(s) to duplicate (default: @)
    -r: string
    --destination(-d): string # The revision(s) to duplicate onto (can be repeated to create a merge commit)
    --insert-after(-A): string # The revision(s) to insert after (can be repeated to create a merge commit)
    --after: string           # The revision(s) to insert after (can be repeated to create a merge commit)
    --insert-before(-B): string # The revision(s) to insert before (can be repeated to create a merge commit)
    --before: string          # The revision(s) to insert before (can be repeated to create a merge commit)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Sets the specified revision as the working-copy revision
  export extern "jj edit" [
    revision: string          # The commit to edit
    -r                        # Ignored (but lets you pass `-r` for consistency with other commands)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show how a change has evolved over time
  export extern "jj evolog" [
    --revision(-r): string
    --limit(-n): string       # Limit number of revisions to show
    -l: string
    --no-graph                # Don't show the graph, show a flat list of revisions
    --template(-T): string    # Render each revision using the given template
    --patch(-p)               # Show patch compared to the previous version of this change
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space        # Ignore whitespace when comparing lines
    --ignore-space-change     # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # File operations
  export extern "jj file" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show the source change for each line of the target file
  export extern "jj file annotate" [
    path: string              # the file to annotate
    --revision(-r): string    # an optional revision to start at
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj file chmod mode" [] {
    [ "n" "x" ]
  }

  # Sets or removes the executable bit for paths in the repo
  export extern "jj file chmod" [
    mode: string@"nu-complete jj file chmod mode"
    --revision(-r): string    # The revision to update
    ...paths: string          # Paths to change the executable bit for
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List files in a revision
  export extern "jj file list" [
    --revision(-r): string    # The revision to list files in
    ...paths: string          # Only list files matching these prefixes (instead of all files)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Print contents of files in a revision
  export extern "jj file show" [
    --revision(-r): string    # The revision to get the file contents from
    ...paths: string          # Paths to print
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Start tracking specified paths in the working copy
  export extern "jj file track" [
    ...paths: string          # Paths to track
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Stop tracking specified paths in the working copy
  export extern "jj file untrack" [
    ...paths: string          # Paths to untrack. They must already be ignored
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List files in a revision (DEPRECATED use `jj file list`)
  export extern "jj files" [
    --revision(-r): string    # The revision to list files in
    ...paths: string          # Only list files matching these prefixes (instead of all files)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update files with formatting fixes or other changes
  export extern "jj fix" [
    --source(-s): string      # Fix files in the specified revision(s) and their descendants. If no revisions are specified, this defaults to the `revsets.fix` setting, or `reachable(@, mutable())` if it is not set
    ...paths: string          # Fix only these paths
    --include-unchanged-files # Fix unchanged files in addition to changed ones. If no paths are specified, all files in the repo will be fixed
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Commands for working with Git remotes and the underlying Git repo
  export extern "jj git" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create a new repo backed by a clone of a Git repo
  export extern "jj git clone" [
    source: string            # URL or path of the Git repo to clone
    destination?: string      # Specifies the target directory for the Jujutsu repository clone. If not provided, defaults to a directory named after the last component of the source URL. The full directory path will be created if it doesn't exist
    --remote: string          # Name of the newly created remote
    --colocate                # Whether or not to colocate the Jujutsu repo with the git repo
    --depth: string           # Create a shallow clone of the given depth
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update the underlying Git repo with changes made in the repo
  export extern "jj git export" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Fetch from a Git remote
  export extern "jj git fetch" [
    --branch(-b): string      # Fetch only some of the branches
    --remote: string          # The remote to fetch from (only named remotes are supported, can be repeated)
    --all-remotes             # Fetch from all remotes
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update repo with changes made in the underlying Git repo
  export extern "jj git import" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create a new Git backed repo
  export extern "jj git init" [
    destination?: string      # The destination directory where the `jj` repo will be created. If the directory does not exist, it will be created. If no directory is given, the current directory is used
    --colocate                # Specifies that the `jj` repo should also be a valid `git` repo, allowing the use of both `jj` and `git` commands in the same directory
    --git-repo: string        # Specifies a path to an **existing** git repository to be used as the backing git repo for the newly created `jj` repo
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Push to a Git remote
  export extern "jj git push" [
    --remote: string          # The remote to push to (only named remotes are supported)
    --bookmark(-b): string    # Push only this bookmark, or bookmarks matching a pattern (can be repeated)
    --all                     # Push all bookmarks (including new and deleted bookmarks)
    --tracked                 # Push all tracked bookmarks (including deleted bookmarks)
    --deleted                 # Push all deleted bookmarks
    --allow-new(-N)           # Allow pushing new bookmarks
    --allow-empty-description # Allow pushing commits with empty descriptions
    --allow-private           # Allow pushing commits that are private
    --revisions(-r): string   # Push bookmarks pointing to these commits (can be repeated)
    --change(-c): string      # Push this commit by creating a bookmark based on its change ID (can be repeated)
    --dry-run                 # Only display what will change on the remote
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Manage Git remotes
  export extern "jj git remote" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Add a Git remote
  export extern "jj git remote add" [
    remote: string            # The remote's name
    url: string               # The remote's URL
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List Git remotes
  export extern "jj git remote list" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Remove a Git remote and forget its bookmarks
  export extern "jj git remote remove" [
    remote: string            # The remote's name
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Rename a Git remote
  export extern "jj git remote rename" [
    old: string               # The name of an existing remote
    new: string               # The desired name for `old`
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Set the URL of a Git remote
  export extern "jj git remote set-url" [
    remote: string            # The remote's name
    url: string               # The desired url for `remote`
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # FOR INTERNAL USE ONLY Interact with git submodules
  export extern "jj git submodule" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Print the relevant contents from .gitmodules. For debugging purposes only
  export extern "jj git submodule print-gitmodules" [
    --revisions(-r): string   # Read .gitmodules from the given revision
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj help keyword" [] {
    [ "bookmarks" "config" "filesets" "glossary" "revsets" "templates" "tutorial" ]
  }

  # Print this message or the help of the given subcommand(s)
  export extern "jj help" [
    ...command: string        # Print help for the subcommand(s)
    --keyword(-k): string@"nu-complete jj help keyword" # Show help for keywords instead of commands
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create a new repo in the given directory
  export extern "jj init" [
    destination?: string      # The destination directory
    --git                     # DEPRECATED: Use `jj git init` Use the Git backend, creating a jj repo backed by a Git repo
    --git-repo: string        # DEPRECATED: Use `jj git init` Path to a git repo the jj repo will be backed by
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Compare the changes of two commits
  export extern "jj interdiff" [
    --from(-f): string        # Show changes from this revision
    --to(-t): string          # Show changes to this revision
    ...paths: string          # Restrict the diff to these paths
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space(-w)    # Ignore whitespace when comparing lines
    --ignore-space-change(-b) # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show revision history
  export extern "jj log" [
    --revisions(-r): string   # Which revisions to show
    ...paths: string          # Show revisions modifying the given paths
    --reversed                # Show revisions in the opposite order (older revisions first)
    --limit(-n): string       # Limit number of revisions to show
    -l: string
    --no-graph                # Don't show the graph, show a flat list of revisions
    --template(-T): string    # Render each revision using the given template
    --patch(-p)               # Show patch
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space        # Ignore whitespace when comparing lines
    --ignore-space-change     # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Create a new, empty change and (by default) edit it in the working copy
  export extern "jj new" [
    ...revisions: string      # Parent(s) of the new change
    -d                        # Ignored (but lets you pass `-d`/`-r` for consistency with other commands)
    --message(-m): string     # The change description to use
    --no-edit                 # Do not edit the newly created change
    --edit                    # No-op flag to pair with --no-edit
    --insert-after(-A): string # Insert the new change after the given commit(s)
    --after: string           # Insert the new change after the given commit(s)
    --insert-before(-B): string # Insert the new change before the given commit(s)
    --before: string          # Insert the new change before the given commit(s)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Move the working-copy commit to the child revision
  export extern "jj next" [
    offset?: string           # How many revisions to move forward. Advances to the next child by default
    --edit(-e)                # Instead of creating a new working-copy commit on top of the target commit (like `jj new`), edit the target commit directly (like `jj edit`)
    --no-edit(-n)             # The inverse of `--edit`
    --conflict                # Jump to the next conflicted descendant
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Commands for working with the operation log
  export extern "jj operation" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Abandon operation history
  export extern "jj operation abandon" [
    operation: string         # The operation or operation range to abandon
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Compare changes to the repository between two operations
  export extern "jj operation diff" [
    --operation: string       # Show repository changes in this operation, compared to its parent
    --op: string              # Show repository changes in this operation, compared to its parent
    --from(-f): string        # Show repository changes from this operation
    --to(-t): string          # Show repository changes to this operation
    --no-graph                # Don't show the graph, show a flat list of modified changes
    --patch(-p)               # Show patch of modifications to changes
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space        # Ignore whitespace when comparing lines
    --ignore-space-change     # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show the operation log
  export extern "jj operation log" [
    --limit(-n): string       # Limit number of operations to show
    -l: string
    --no-graph                # Don't show the graph, show a flat list of operations
    --template(-T): string    # Render each operation using the given template
    --op-diff                 # Show changes to the repository at each operation
    --patch(-p)               # Show patch of modifications to changes (implies --op-diff)
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space        # Ignore whitespace when comparing lines
    --ignore-space-change     # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj operation restore what" [] {
    [ "repo" "remote-tracking" ]
  }

  # Create a new operation that restores the repo to an earlier state
  export extern "jj operation restore" [
    operation: string         # The operation to restore to
    --what: string@"nu-complete jj operation restore what" # What portions of the local state to restore (can be repeated)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show changes to the repository in an operation
  export extern "jj operation show" [
    operation?: string        # Show repository changes in this operation, compared to its parent(s)
    --no-graph                # Don't show the graph, show a flat list of modified changes
    --patch(-p)               # Show patch of modifications to changes
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space        # Ignore whitespace when comparing lines
    --ignore-space-change     # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj operation undo what" [] {
    [ "repo" "remote-tracking" ]
  }

  # Create a new operation that undoes an earlier operation
  export extern "jj operation undo" [
    operation?: string        # The operation to undo
    --what: string@"nu-complete jj operation undo what" # What portions of the local state to restore (can be repeated)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Parallelize revisions by making them siblings
  export extern "jj parallelize" [
    ...revisions: string      # Revisions to parallelize
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Change the working copy revision relative to the parent revision
  export extern "jj prev" [
    offset?: string           # How many revisions to move backward. Moves to the parent by default
    --edit(-e)                # Edit the parent directly, instead of moving the working-copy commit
    --no-edit(-n)             # The inverse of `--edit`
    --conflict                # Jump to the previous conflicted ancestor
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Move revisions to different parent(s)
  export extern "jj rebase" [
    --branch(-b): string      # Rebase the whole branch relative to destination's ancestors (can be repeated)
    --source(-s): string      # Rebase specified revision(s) together with their trees of descendants (can be repeated)
    --revisions(-r): string   # Rebase the given revisions, rebasing descendants onto this revision's parent(s)
    --destination(-d): string # The revision(s) to rebase onto (can be repeated to create a merge commit)
    --insert-after(-A): string # The revision(s) to insert after (can be repeated to create a merge commit)
    --after: string           # The revision(s) to insert after (can be repeated to create a merge commit)
    --insert-before(-B): string # The revision(s) to insert before (can be repeated to create a merge commit)
    --before: string          # The revision(s) to insert before (can be repeated to create a merge commit)
    --skip-empty              # Deprecated. Use --skip-emptied instead
    --skip-emptied            # If true, when rebasing would produce an empty commit, the commit is abandoned. It will not be abandoned if it was already empty before the rebase. Will never skip merge commits with multiple non-empty parents
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Resolve a conflicted file with an external merge tool
  export extern "jj resolve" [
    --revision(-r): string
    --list(-l)                # Instead of resolving one conflict, list all the conflicts
    --tool: string            # Specify 3-way merge tool to be used
    ...paths: string          # Restrict to these paths when searching for a conflict to resolve. We will attempt to resolve the first conflict we can find. You can use the `--list` argument to find paths to use here
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Restore paths from another revision
  export extern "jj restore" [
    ...paths: string          # Restore only these paths (instead of all paths)
    --from(-f): string        # Revision to restore from (source)
    --to(-t): string          # Revision to restore into (destination)
    --changes-in(-c): string  # Undo the changes in a revision as compared to the merge of its parents
    --revision(-r): string    # Prints an error. DO NOT USE
    --restore-descendants     # Preserve the content (not the diff) when rebasing descendants
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # A dummy command that accepts any arguments
  export extern "jj revert" [
    ..._args: string
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show the current workspace root directory
  export extern "jj root" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # (**Stub**, does not work yet) Run a command across a set of revisions.
  export extern "jj run" [
    shell_command: string     # The command to run across all selected revisions
    --revisions(-r): string   # The revisions to change
    -x                        # A no-op option to match the interface of `git rebase -x`
    --jobs(-j): string        # How many processes should run in parallel, uses by default all cores
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show commit description and changes in a revision
  export extern "jj show" [
    revision?: string         # Show changes in this revision, compared to its parent(s)
    -r                        # Ignored (but lets you pass `-r` for consistency with other commands)
    --template(-T): string    # Render a revision using the given template
    --summary(-s)             # For each path, show only whether it was modified, added, or deleted
    --stat                    # Show a histogram of the changes
    --types                   # For each path, show only its type before and after
    --name-only               # For each path, show only its path
    --git                     # Show a Git-format diff
    --color-words             # Show a word-level diff with changes indicated only by color
    --tool: string            # Generate diff by external command
    --context: string         # Number of lines of context to show
    --ignore-all-space        # Ignore whitespace when comparing lines
    --ignore-space-change     # Ignore changes in amount of whitespace when comparing lines
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Simplify parent edges for the specified revision(s)
  export extern "jj simplify-parents" [
    --source(-s): string      # Simplify specified revision(s) together with their trees of descendants (can be repeated)
    --revisions(-r): string   # Simplify specified revision(s) (can be repeated)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Manage which paths from the working-copy commit are present in the working copy
  export extern "jj sparse" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Start an editor to update the patterns that are present in the working copy
  export extern "jj sparse edit" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List the patterns that are currently present in the working copy
  export extern "jj sparse list" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Reset the patterns to include all files in the working copy
  export extern "jj sparse reset" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update the patterns that are present in the working copy
  export extern "jj sparse set" [
    --add: string             # Patterns to add to the working copy
    --remove: string          # Patterns to remove from the working copy
    --clear                   # Include no files in the working copy (combine with --add)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Split a revision in two
  export extern "jj split" [
    --interactive(-i)         # Interactively choose which parts to split
    --tool: string            # Specify diff editor to be used (implies --interactive)
    --revision(-r): string    # The revision to split
    --parallel(-p)            # Split the revision into two parallel revisions instead of a parent and child
    ...paths: string          # Files matching any of these filesets are put in the first commit
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Move changes from a revision into another revision
  export extern "jj squash" [
    --revision(-r): string    # Revision to squash into its parent (default: @)
    --from(-f): string        # Revision(s) to squash from (default: @)
    --into(-t): string        # Revision to squash into (default: @)
    --to: string              # Revision to squash into (default: @)
    --message(-m): string     # The description to use for squashed revision (don't open editor)
    --use-destination-message(-u) # Use the description of the destination revision and discard the description(s) of the source revision(s)
    --interactive(-i)         # Interactively choose which parts to squash
    --tool: string            # Specify diff editor to be used (implies --interactive)
    ...paths: string          # Move only changes to these paths (instead of all paths)
    --keep-emptied(-k)        # The source revision will not be abandoned
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show high-level repo status
  export extern "jj status" [
    ...paths: string          # Restrict the status display to these paths
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Manage tags
  export extern "jj tag" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List tags
  export extern "jj tag list" [
    ...names: string          # Show tags whose local name matches
    --template(-T): string    # Render each tag using the given template
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Infrequently used commands such as for generating shell completions
  export extern "jj util" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj util completion shell" [] {
    [ "bash" "elvish" "fish" "nushell" "power-shell" "zsh" ]
  }

  # Print a command-line-completion script
  export extern "jj util completion" [
    shell?: string@"nu-complete jj util completion shell"
    --bash                    # Deprecated. Use the SHELL positional argument instead
    --fish                    # Deprecated. Use the SHELL positional argument instead
    --zsh                     # Deprecated. Use the SHELL positional argument instead
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Print the JSON schema for the jj TOML config format
  export extern "jj util config-schema" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Execute an external command via jj
  export extern "jj util exec" [
    command: string           # External command to execute
    ...args: string           # Arguments to pass to the external command
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Run backend-dependent garbage collection
  export extern "jj util gc" [
    --expire: string          # Time threshold
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Print a ROFF (manpage)
  export extern "jj util mangen" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Print the CLI help for all subcommands in Markdown
  export extern "jj util markdown-help" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj undo what" [] {
    [ "repo" "remote-tracking" ]
  }

  # Undo an operation (shortcut for `jj op undo`)
  export extern "jj undo" [
    operation?: string        # The operation to undo
    --what: string@"nu-complete jj undo what" # What portions of the local state to restore (can be repeated)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Move changes from a revision's parent into the revision
  export extern "jj unsquash" [
    --revision(-r): string
    --interactive(-i)         # Interactively choose which parts to unsquash
    --tool: string            # Specify diff editor to be used (implies --interactive)
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Stop tracking specified paths in the working copy
  export extern "jj untrack" [
    ...paths: string          # Paths to untrack. They must already be ignored
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Display version information
  export extern "jj version" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Commands for working with workspaces
  export extern "jj workspace" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  def "nu-complete jj workspace add sparse_patterns" [] {
    [ "copy" "full" "empty" ]
  }

  # Add a workspace
  export extern "jj workspace add" [
    destination: string       # Where to create the new workspace
    --name: string            # A name for the workspace
    --revision(-r): string    # A list of parent revisions for the working-copy commit of the newly created workspace. You may specify nothing, or any number of parents
    --sparse-patterns: string@"nu-complete jj workspace add sparse_patterns" # How to handle sparse patterns when creating a new workspace
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Stop tracking a workspace's working-copy commit in the repo
  export extern "jj workspace forget" [
    ...workspaces: string     # Names of the workspaces to forget. By default, forgets only the current workspace
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # List workspaces
  export extern "jj workspace list" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Renames the current workspace
  export extern "jj workspace rename" [
    new_workspace_name: string # The name of the workspace to update to
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Show the current workspace root directory
  export extern "jj workspace root" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

  # Update a workspace that has become stale
  export extern "jj workspace update-stale" [
    --repository(-R): string  # Path to repository to operate on
    --ignore-working-copy     # Don't snapshot the working copy, and don't update it
    --ignore-immutable        # Allow rewriting immutable commits
    --at-operation: string    # Operation to load the repo at
    --at-op: string           # Operation to load the repo at
    --debug                   # Enable debug logging
    --color: string           # When to colorize output (always, never, debug, auto)
    --quiet                   # Silence non-primary command output
    --no-pager                # Disable the pager
    --config: string          # Additional configuration options (can be repeated)
    --config-toml: string     # Additional configuration options (can be repeated) (DEPRECATED)
    --config-file: string     # Additional configuration files (can be repeated)
    --help(-h)                # Print help (see more with '--help')
  ]

}

export use completions *
