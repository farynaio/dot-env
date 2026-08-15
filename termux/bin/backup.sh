#!/bin/bash

set -euo pipefail

# Perform backup of Photos and Books directories stored in Android protected area on EXTERNAL_DRIVE_DIR volume mounted with termux-setup-storage.
# Verify EXTERNAL_DRIVE_DIR is correct

readonly BACKUP_DIR=$(date +%F)
readonly BACKUP_NAME="medical-data"
readonly EXTERNAL_DRIVE_DIR="${HOME}/storage/external-1"
readonly BACKUP_WORK_DIR="${EXTERNAL_DRIVE_DIR}/Backup"
readonly BACKUP_SAF_DIR="${EXTERNAL_DRIVE_DIR}/Backup/SAF-tmp"
readonly DEST_DIR="${BACKUP_WORK_DIR}/${BACKUP_DIR}"
readonly PASS_FILE_NAME="documents"

# TODO move to .bashrc
# Add SAF folder URIs here to be included in encrypted archive.
# Also add parametrized saf_folder_copy call add the end
# readonly SAF_BOOKS_URI="content://com.android.externalstorage.documents/tree/0084-3000%3ABooks/document/0084-3000%3ABooks"
readonly SAF_BOOKS_URI="content://com.android.externalstorage.documents/tree/0084-3000%3AFoo/document/0084-3000%3AFoo"
readonly SAF_PICTURES_URI="content://com.android.externalstorage.documents/tree/0084-3000%3APictures/document/0084-3000%3APictures"
readonly SAF_DOCUMENTS_PASS_URI="content://com.android.externalstorage.documents/tree/0084-3000%3ADocuments/document/0084-3000%3ADocuments%2Fpass"

# Files and directories in home dir, to be included in the backup
DIRS_TO_BACKUP_HOME=("Documents" ".ssh")

# Verify jq is installed
if ! command -v jq &> /dev/null; then
  echo "ERROR: 'jq' is not installed."
  echo "Please install it by running: pkg install jq"
  exit 1
fi

# Called on any exit
cleanup() {
  # Backup original exit code
  local exit_code=$?
  echo "Cleaning up..."
  unset PASSWORD
  unset PASSWORD_CONFIRM
  rm -rf BACKUP_SAF_DIR
  return $exit_code
}

trap cleanup EXIT

# Provide password in the secure way, that will be used for GPG symmetric encryption of the backup
read -s -p "Enter encryption password: " PASSWORD
echo ""
read -s -p "Confirm password: " PASSWORD_CONFIRM
echo ""

if [[ "$PASSWORD" != "$PASSWORD_CONFIRM" ]]; then
  echo "[ERROR] Passwords do not match."
  exit 1
fi

mkdir -p "$DEST_DIR"

encrypt() {
  local output_file="$1"
  gpg --batch --yes --passphrase-fd 3 --symmetric --cipher-algo AES256 -o "$output_file" 3<<<"$PASSWORD"
}

saf_folder_copy() {
  local src_uri="$1"
  local dest_subdir="$2"
  local dest_path="${BACKUP_SAF_DIR}/${dest_subdir}"
  local extract_filename="${3:-}"

  # Verify is $src_uri correct
  echo "Validating ${src_uri}..."
  if ! termux-saf-ls "${src_uri}" > /dev/null 2>&1; then
    echo "ERROR: Cannot access ${src_uri}."
    echo "Please check if the URI is correct."
    exit 1
  fi

  mkdir -p "$dest_path"

  termux-saf-ls "$src_uri" | jq -c '.[]' | while read -r entry; do
    local file_uri=$(echo "$entry" | jq -r '.uri')
    local file_name=$(echo "$entry" | jq -r '.name')
    local file_type=$(echo "$entry" | jq -r '.type')

    # Skip directories
    if [ "$file_type" = "vnd.android.document/directory" ]; then
      echo "Skipping directory: ${file_name}"
      continue
    fi

    echo "Copying: ${file_name}..."
    if termux-saf-read "${file_uri}" > "${dest_path}/${file_name}"; then
      echo " -> Success: ${file_name}"
    else
      echo " -> Failed: ${file_name}"
    fi
  done

  if [[ -n "$extract_filename" ]]; then
    local cwd=`pwd`
    cd "$BACKUP_SAF_DIR"
    ls
tar czf - "$dest_subdir" | encrypt "${extract_filename}.tar.gz.gpg"
    mv "${extract_filename}.tar.gz.gpg" "$DEST_DIR"
    cd "$cwd"
  fi
}

# excludes symbolic links
create_encrypted_archive() {
  local src="$1"
  local dest="$2"
  local output_file="${dest}/${BACKUP_NAME}.tar.gz.gpg"
  local dirs_to_backup=$DIRS_TO_BACKUP_HOME

  dirs_to_backup+=("${BACKUP_SAF_DIR}/*")

  mkdir -p "$dest"

  echo "Creating encrypted archive with specific inclusions..."
  echo "Source Base: ${src}"
  echo "Including: ${dirs_to_backup[*]}"

  local find_args=("." -mindepth 1 -not -type l \( -false) # Start with a false OR condition

  for item in "${dirs_to_backup[@]}"; do
    # Check if item is a directory or file to include content recursively if dir
    # Using -path allows including specific sub-paths relative to src
    find_args+=( -o -path "./${item}" -o -path "./${item}/*" )
  done
  find_args+=( \) -print0)

  # 2. Pipeline: Find -> Tar -> GPG
  # tar --null --files-from=- reads the null-separated list from find
  # --no-recursion is NOT used here because if 'item' is a dir, we WANT its contents.
  # However, find already expands directories if we use -path "dir/*",
  # but to be safe and include the directory structure itself, we rely on tar's default behavior
  # when given a directory in the list, BUT we filtered symlinks in find.

  # Refined Strategy: Use find to list EXACT matches for the includes,
  # then let tar handle the recursion for directories found.
  # Since we added -not -type l to find, symlinks are gone.

  local cwd=`pwd`
  cd ~

  if find "${find_args[@]}" | \
      tar --null --files-from=- -czf - | encrypt "$output_file"; then
    echo "[SUCCESS] Archive created at ${output_file}"
  else
    echo "[ERROR] Archive creation failed."
    return 1
  fi

  cd "$cwd"
}

# Add more saf_folder_copy calls here
saf_folder_copy "$SAF_BOOKS_URI" "Books"
saf_folder_copy "$SAF_PICTURES_URI" "Pictures"
saf_folder_copy "$SAF_DOCUMENTS_PASS_URI" "Pass" "$PASS_FILE_NAME"

create_encrypted_archive "$HOME" "$DEST_DIR"

echo "Done"