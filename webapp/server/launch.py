"""
launch.py -- StandardEyes starter.

Picks a FREE port before starting, so this app can never collide with another
copy (or with a different app) already using a port. The browser is opened on
the port we actually bound, and the port is printed in the console.
"""
from __future__ import annotations

import os
import socket
import sys
import threading
import time
import webbrowser

SERVER_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, SERVER_DIR)

# Deliberately NOT 8000/8010 - those are commonly taken by other local apps.
PREFERRED = [8760, 8761, 8762, 8763, 8764, 8765]
RELEASES_URL = "https://standardeyes.github.io"

_CONSOLE_HANDLERS: list = []     # ctypes callbacks must outlive this function


def install_console_handler(cleanup) -> bool:
    """Windows ends a console process without running atexit hooks when its
    window is closed (and on Ctrl+Break, logoff and shutdown), which would
    leave the R child running and this window's preview staging - patient-
    level files - in the app folder until the next start. Run `cleanup`
    first, then let the default handling end the process. Ctrl+C also gets
    it before uvicorn's own graceful shutdown (cleanup is idempotent)."""
    if os.name != "nt":
        return False
    try:
        import ctypes
        from ctypes import wintypes

        events = (0, 1, 2, 5, 6)     # CTRL_C, CTRL_BREAK, CTRL_CLOSE, CTRL_LOGOFF, CTRL_SHUTDOWN

        @ctypes.WINFUNCTYPE(wintypes.BOOL, wintypes.DWORD)
        def _on_console_event(event):
            if event in events:
                try:
                    cleanup()
                except Exception:  # noqa: BLE001  (never block the shutdown)
                    pass
            return False             # not handled: the default handler ends the process

        ok = bool(ctypes.WinDLL("kernel32").SetConsoleCtrlHandler(_on_console_event, True))
        if ok:
            _CONSOLE_HANDLERS.append(_on_console_event)
        return ok
    except Exception:  # noqa: BLE001
        return False


def pick_port() -> int:
    for p in PREFERRED:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            try:
                s.bind(("127.0.0.1", p))
                return p
            except OSError:
                continue
    # nothing preferred was free: let the OS choose any free port
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def main() -> int:
    try:
        import uvicorn
        from app import APP_VERSION, PUBLIC_VERSION, shutdown_cleanup
    except ImportError as e:
        if "DLL load failed" in str(e):
            # the file is there but Windows refused to load it: Smart App
            # Control / AppLocker / antivirus block unsigned components
            print("ERROR: Windows security blocked a component of StandardEyes:", e)
            print("Smart App Control, AppLocker or an antivirus program refused to load it.")
            print("Ask your IT department to allow this folder, or turn off Smart App Control")
            print("(Windows Security > App & browser control). Copying the folder again will not help.")
        else:
            print("ERROR: the bundled runtime is incomplete:", e)
            print("Copy or extract the WHOLE StandardEyes folder again, keeping its subfolders.")
        input("Press Enter to close...")
        return 1
    except OSError as e:
        # the app keeps presets, run settings and temporary results in its own
        # folder, so a read-only location (C:\Program Files, a locked share)
        # stops it here
        print("ERROR: StandardEyes cannot write to its own folder:", e)
        print("Move the whole StandardEyes folder somewhere you can write to,")
        print(r"for example Documents or C:\StandardEyes - not C:\Program Files.")
        input("Press Enter to close...")
        return 1
    except Exception as e:  # noqa: BLE001
        print("ERROR: StandardEyes could not start:", repr(e))
        input("Press Enter to close...")
        return 1

    install_console_handler(shutdown_cleanup)
    port = pick_port()
    url = f"http://127.0.0.1:{port}"

    banner = (
        "=" * 62
        + "\n  StandardEyes - refractive surgery analysis"
        + "\n  By Noyman DBE, Mimouni M, Safir M"
        + f"\n  v{PUBLIC_VERSION} (build {APP_VERSION})"
        + f"\n  Newer versions: {RELEASES_URL}\n"
        + "=" * 62
        + f"\n\n  Opening: {url}"
        + "\n\n  Keep THIS WINDOW OPEN while you work."
        + "\n  Closing it stops the app.\n"
    )
    print(banner, flush=True)

    # open the browser once the server is actually accepting connections
    def opener():
        for _ in range(80):
            time.sleep(0.25)
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.settimeout(0.25)
                if s.connect_ex(("127.0.0.1", port)) == 0:
                    break
        webbrowser.open(url)

    threading.Thread(target=opener, daemon=True).start()

    uvicorn.run("app:app", host="127.0.0.1", port=port, log_level="warning")
    return 0


if __name__ == "__main__":
    sys.exit(main())
