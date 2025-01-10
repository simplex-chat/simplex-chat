#!/bin/bash

set -e

# Ensure script is called with two arguments
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <sqlite_db_prefix> <output_folder>"
    exit 1
fi

# Arguments
DB_PREFIX="$1"
OUTPUT_FOLDER="$2"

# Function to process a database and export its tables to CSV
process_db() {
    local db_suffix="$1"  # Suffix for the database
    local folder="$2"     # Subfolder name

    # Create output folder and subfolder if they don't exist
    mkdir -p "$OUTPUT_FOLDER/$folder"

    # Concatenate the prefix and suffix to form the full database path
    local db_path="${DB_PREFIX}_${db_suffix}.db"

    # Check if the database file exists
    if [ ! -f "$db_path" ]; then
        echo "Error: Database file '$db_path' not found. Exiting."
        exit 1
    fi

    # Build list of non-service tables
    local tables=$(sqlcipher "$db_path" -cmd ".mode list" -noheader "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' AND name != 'migrations';")

    echo $tables

    # Export each table to a CSV file
    for table in $tables; do
        echo "Exporting table '$table' from '$db_path' to '$OUTPUT_FOLDER/$folder/$table.csv'..."
        sqlcipher "$db_path" -header -csv "SELECT * FROM $table;" > "$OUTPUT_FOLDER/$folder/$table.csv"
    done
}

process_db "agent" "agent_db"

process_db "chat" "chat_db"

echo "Export complete."
