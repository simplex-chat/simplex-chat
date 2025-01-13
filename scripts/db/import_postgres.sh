#!/bin/bash

set -e

# Ensure script is called with three arguments
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <exported_tables_folder> <postgres_user> <postgres_database>"
    exit 1
fi

# Arguments
EXPORT_FOLDER="$1"
POSTGRES_USER="$2"
POSTGRES_DB="$3"

# Function to process a folder and import tables into the corresponding schema
import_tables() {
    local folder="$1"  # Subfolder name
    local schema="$2"  # Schema name

    # Get the list of tables by removing the .csv extension from file names in the subfolder
    local table_files=("$EXPORT_FOLDER/$folder"/*.csv)
    local tables=()
    for file in "${table_files[@]}"; do
        tables+=("$(basename "$file" .csv)")
    done

    echo "Will process the following tables in schema '$schema': ${tables[*]}"

    # Import each table into the schema
    for table in "${tables[@]}"; do
        echo "Processing table '$table' in schema '$schema'..."

        # Disable triggers
        psql "user=$POSTGRES_USER dbname=$POSTGRES_DB options=--search_path=$schema" -c "ALTER TABLE $table DISABLE TRIGGER ALL;"

        # Import data from the CSV file
        psql "user=$POSTGRES_USER dbname=$POSTGRES_DB options=--search_path=$schema" -c \
            "COPY $table FROM '$EXPORT_FOLDER/$folder/$table.csv' DELIMITER ',' CSV HEADER NULL 'NULL';"

        # Enable triggers
        psql "user=$POSTGRES_USER dbname=$POSTGRES_DB options=--search_path=$schema" -c "ALTER TABLE $table ENABLE TRIGGER ALL;"
    done
}

import_tables "agent_db" "agent_schema"

import_tables "chat_db" "chat_schema"

echo "Import complete."
