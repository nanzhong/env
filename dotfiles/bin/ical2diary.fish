#!/usr/bin/env fish

if set -q CALENDAR_URL
    echo "Fetching calendar from: $CALENDAR_URL"
else
    echo "Failed. CALENDAR_URL not set." >&2
    exit 1
end

if set -q DIARY_FILE
    echo "Updating diary file: $DIARY_FILE"
else
    echo "Failed. DIARY_FILE not set." >&2
    exit 1
end

if ! set -q TMP_DIR
    set TMP_DIR /tmp/fetch-calendar
end
echo "Using tmp dir: $TMP_DIR." >&2

mkdir -p "$TMP_DIR"
curl -sLo "$TMP_DIR/cal.ics" "$CALENDAR_URL"

emacs --batch \
      --eval "(require 'calendar)" \
      --eval "(calendar-set-date-style 'iso)" \
      --eval "(icalendar-import-file \"$TMP_DIR/cal.ics\" \"$TMP_DIR/diary\")"
mv "$TMP_DIR/diary" "$DIARY_FILE"
