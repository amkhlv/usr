Using notmuch to deduplicate email files
========================================

    notmuch search --output=files --duplicate=2 '*' | while read N; do rm "$N"; done

