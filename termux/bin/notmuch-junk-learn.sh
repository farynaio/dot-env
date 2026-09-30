#!/bin/bash

# Teach rspamd server junk messages after tagging with notmuch tag:junk.

# Termux: no /tmp, use $HOME/.tmp
TMP="${TMPDIR}/notmuch-junk-learn"
mkdir -p "$TMP"

# Enable password for rspamd
RSPAMD_PASS=$(pass show vps/rspamd-api)

SSH_TARGET="vps1"

# Find junk messages not yet sent to rspamd
ids=$(notmuch search --output=messages 'tag:junk and not tag:gmail and not tag:junk-learned' 2>/dev/null)

[ -z "$ids" ] && exit 0

# Ope SSH tunnel
ssh -o ConnectTimeout=5 -o BatchMode=yes \
    -L 11334:127.0.0.1:11334 -N "$SSH_TARGET" &
TUNNEL_PID=$!
sleep 2

count=0

for id in $ids; do
    notmuch show --format=raw "$id" > "$TMP/learn_msg.eml"

    result=$(curl -s -o /dev/null -w "%{http_code}" \
        -H "Password: $RSPAMD_PASS" \
        --data-binary @"$TMP/learn_msg.eml" \
        http://127.0.0.1:11334/learnspam)

    if [ "$result" = "200" ] || [ "$result" = "208" ]; then
        notmuch tag +junk-learned -- "$id"
        count=$((count + 1))
    fi
done

rm -f "$TMP/learn_msg.eml"
kill $TUNNEL_PID 2>/dev/null

echo "Learned ${count} junk messages"