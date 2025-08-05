# Quick Umlaut

A lightweight Windows program that uses low-level keyboard hooks to enable German umlaut shortcuts.  
You can type umlauts while keeping your QWERTY layout unchanged.

## User Interface Preview

![App Preview](https://raw.githubusercontent.com/hwahyeon/quick-umlaut/refs/heads/readme/images/preview.png)

## Installation

Download the latest release from the [Releases page](https://github.com/hwahyeon/quick-umlaut/releases/tag/v1.0.0).

## Key Combinations

| Key Combo                | Output |
| ------------------------ | ------ |
| Ctrl + Alt + (Shift) + A | ä / Ä  |
| Ctrl + Alt + (Shift) + O | ö / Ö  |
| Ctrl + Alt + (Shift) + U | ü / Ü  |
| Ctrl + Alt + (Shift) + S | ß / ẞ  |
| Ctrl + Alt + [           | „      |
| Ctrl + Alt + ]           | “      |

## Usage

Once the app is running, press the key combinations listed above to insert umlaut characters.

### Example

In any text editor (e.g., Notepad), press:

```
Ctrl + Alt + U → ü
Ctrl + Alt + Shift + A → Ä
```

No need to change your keyboard layout or language settings.

## Build Instructions

Requires GCC (MinGW) and `gdi32`:

```bash
gcc src/main.c src/gui.c src/keymap.c -o build/app.exe -lgdi32 -municode -mwindows
```

Or use the included VS Code `tasks.json` for building and running.

## Compatibility

- OS: Windows only (due to use of Windows API and keyboard hooks)
- Tested on: Windows 10, Windows 11
