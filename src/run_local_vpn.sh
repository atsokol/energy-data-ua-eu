#!/usr/bin/env bash
#============================================================================
# run_local_vpn.sh — Download BM UA data via Ukrainian VPN
#
# Usage:
#   ./run_local_vpn.sh              # uses Apple Shortcuts for VPN, then pushes
#   ./run_local_vpn.sh --no-vpn     # skip VPN toggle (already connected)
#   ./run_local_vpn.sh --no-commit  # skip git commit + push after download
#
# VPN options (pick ONE during setup — see README):
#   1. Apple Shortcuts  — create "ClearVPN Connect UA" and "ClearVPN Disconnect"
#      shortcuts in the Shortcuts app, then this script calls them automatically.
#   2. Manual           — run with --no-vpn after manually activating VPN.
#============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# ── Parse flags ──────────────────────────────────────────────────────────────
SKIP_VPN=false
DO_COMMIT=true

for arg in "$@"; do
  case "$arg" in
    --no-vpn)     SKIP_VPN=true ;;
    --no-commit)  DO_COMMIT=false ;;
    *)            echo "Unknown flag: $arg"; exit 1 ;;
  esac
done

# ── Helper: check if Apple Shortcut exists ───────────────────────────────────
shortcut_exists() {
  shortcuts list 2>/dev/null | grep -qF "$1"
}

# ── VPN connect ──────────────────────────────────────────────────────────────
vpn_connect() {
  if [ "$SKIP_VPN" = true ]; then
    echo "⏭  --no-vpn: skipping VPN activation"
    return
  fi

  if shortcut_exists "ClearVPN Connect UA"; then
    echo "🔐 Activating ClearVPN → Ukraine via Apple Shortcut..."
    shortcuts run "ClearVPN Connect UA"
    echo "⏳ Waiting 8s for VPN tunnel to establish..."
    sleep 8
  else
    echo "⚠️  Apple Shortcut 'ClearVPN Connect UA' not found."
    echo "   Create it in Shortcuts.app: add ClearVPN 'Teleport me to' action → pick Ukraine"
    echo "   Or rerun with --no-vpn"
    exit 1
  fi
}

# ── VPN disconnect ───────────────────────────────────────────────────────────
vpn_disconnect() {
  if [ "$SKIP_VPN" = true ]; then
    return
  fi

  if shortcut_exists "Turn off VPN"; then
    echo "🔓 Deactivating ClearVPN via Apple Shortcut..."
    shortcuts run "Turn off VPN"
  else
    echo "⚠️  Shortcut 'Turn off VPN' not found — VPN still active"
  fi
}

# ── Main ─────────────────────────────────────────────────────────────────────
cd "$PROJECT_DIR"
echo "📂 Working directory: $PROJECT_DIR"
echo ""

# Step 1: Connect VPN
vpn_connect

# Step 2: Run BM UA download
echo ""
echo "📥 Running BM UA download..."
RSCRIPT_EXIT=0
Rscript src/tasks/task_bm_ua.R 2>&1 || RSCRIPT_EXIT=$?

# Step 3: Disconnect VPN (always, even if R failed)
echo ""
vpn_disconnect

# Step 4: Re-run transform to pick up new BM data
if [ "$RSCRIPT_EXIT" -eq 0 ]; then
  echo ""
  echo "🔄 Re-running transform with updated data..."
  Rscript src/tasks/task_transform.R 2>&1 || true
fi

# Step 5: Optionally commit
if [ "$DO_COMMIT" = true ] && [ "$RSCRIPT_EXIT" -eq 0 ]; then
  echo ""
  echo "📤 Committing updated data..."
  git add data/data_raw/BM_UA.csv data/data_output/*.csv data/data_output/*.parquet 2>/dev/null || true
  if ! git diff --staged --quiet 2>/dev/null; then
    git commit -m "Update BM UA data $(date +%Y-%m-%d)"
    git push
    echo "✅ Committed and pushed"
  else
    echo "ℹ️  No changes to commit"
  fi
fi

# Step 6: Report
echo ""
if [ "$RSCRIPT_EXIT" -eq 0 ]; then
  echo "✅ BM UA update completed successfully"
else
  echo "❌ BM UA update failed (exit code $RSCRIPT_EXIT)"
  exit "$RSCRIPT_EXIT"
fi
