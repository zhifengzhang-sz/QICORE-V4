#!/bin/bash

# QiCore v4.0 Diary Entry Creator
# Usage: ./new_entry.sh [YYYY-MM-DD] or ./new_entry.sh (for today)

# Get date - use argument if provided, otherwise use today
if [ $# -eq 1 ]; then
    DATE_INPUT="$1"
    # Validate date format
    if ! date -d "$DATE_INPUT" >/dev/null 2>&1; then
        echo "Error: Invalid date format. Use YYYY-MM-DD"
        exit 1
    fi
    DATE_STR="$DATE_INPUT"
else
    DATE_STR=$(date +%Y-%m-%d)
fi

# Parse date components
YEAR=$(date -d "$DATE_STR" +%Y)
MONTH=$(date -d "$DATE_STR" +%m)
DAY=$(date -d "$DATE_STR" +%d)
MONTH_NAME=$(date -d "$DATE_STR" +%B)

# Create directory structure
DIARY_DIR="docs/diary/$YEAR/$MONTH"
mkdir -p "$DIARY_DIR"

# Create filename
FILENAME="$YEAR.$MONTH.$DAY.md"
FILEPATH="$DIARY_DIR/$FILENAME"

# Check if file already exists
if [ -f "$FILEPATH" ]; then
    echo "Entry for $DATE_STR already exists: $FILEPATH"
    echo "Opening existing file..."
else
    echo "Creating new diary entry for $DATE_STR..."
    
    # Create the file with template
    cat > "$FILEPATH" << EOF
# $MONTH_NAME $DAY, $YEAR

## Status: [Brief Status]

### Learned Today
- 
- 

### Completed
- 
- 

### Key Insight
[Main insight from today's work]

### Next
[What's next]

---
**Status**: [Current Status]
EOF
    
    echo "Created new diary entry: $FILEPATH"
fi

# Open the file in default editor if available
if command -v code >/dev/null 2>&1; then
    echo "Opening in VS Code..."
    code "$FILEPATH"
elif command -v nano >/dev/null 2>&1; then
    echo "Opening in nano..."
    nano "$FILEPATH"
else
    echo "File created. Open manually: $FILEPATH"
fi 