#!/bin/bash -i

set -euo pipefail

# Perform backup of Photos, Books and other directories stored in Android protected area, and user storage. Store it on EXTERNAL_DRIVE_DIR volume mounted with termux-setup-storage.
# Verify EXTERNAL_DRIVE_DIR and other variables values are correct before proceeding
# create global exported variables like BACKUP_NAME and other used here in .bashrc

readonly BACKUP_DIR=$(date +%F)
readonly EXTERNAL_DRIVE_DIR="${HOME}/storage/external-1"
readonly BACKUP_WORK_DIR="${EXTERNAL_DRIVE_DIR}/Backup"
readonly BACKUP_SAF_DIR="${EXTERNAL_DRIVE_DIR}/Backup/SAF-tmp"
readonly DEST_DIR="${BACKUP_WORK_DIR}/${BACKUP_DIR}"

# Files and directories in home dir, to be included in the backup
DIRS_TO_BACKUP_HOME_ENCRYPTED=("Documents" ".ssh" "storage/shared/Documents")

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

  local cwd=`pwd`
  cd "$BACKUP_SAF_DIR"

  if [[ -n "$extract_filename" ]]; then
    tar czf - "$dest_subdir" | encrypt "${extract_filename}.tar.gz.gpg"
    mv "${extract_filename}.tar.gz.gpg" "$DEST_DIR"
    rm -rf "$dest_subdir"
  else
    mv "$dest_subdir" "$DEST_DIR"
  fi

  cd "$cwd"
}

# excludes symbolic links
create_encrypted_archive() {
  local src="$1"
  local dest="$2"
  local output_file="${dest}/${BACKUP_NAME}.tar.gz.gpg"
  local dirs_to_backup=("${DIRS_TO_BACKUP_HOME_ENCRYPTED[@]}")

  mkdir -p "$dest"

  local cwd=`pwd`
  cd ~

  if tar --null -czf - "${dirs_to_backup[@]}" | encrypt "$output_file"; then
    echo "[SUCCESS] Archive created at ${output_file}"
  else
    echo "[ERROR] Archive creation failed."
    return 1
  fi

  cd "$cwd"
}

# Add more saf_folder_copy calls here
# SAF_* variables are declared in .bashrc
# TODO probably better to organize these as a loop through array
# saf_folder_copy "$BACKUP_SAF_BOOKS_URI" "Books" "$BACKUP_BOOKS_FILENAME"
saf_folder_copy "$BACKUP_SAF_PASS_URI" "Pass" "$BACKUP_PASS_FILENAME"
saf_folder_copy "$BACKUP_SAF_VPN_URI" "VPN"
saf_folder_copy "$BACKUP_SAF_EXPORTS_URI" "Exports"

create_encrypted_archive "$HOME" "$DEST_DIR"

echo "Done"