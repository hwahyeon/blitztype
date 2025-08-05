#include "keymap.h"
#include <stdio.h>

HHOOK hHook;  // Global hook handle

// Function to send Unicode characters
void SendUnicodeChar(wchar_t c) {
    INPUT input[2] = {0};

    // Key press event
    input[0].type = INPUT_KEYBOARD;
    input[0].ki.wScan = c;
    input[0].ki.dwFlags = KEYEVENTF_UNICODE;

    // Key release event
    input[1].type = INPUT_KEYBOARD;
    input[1].ki.wScan = c;
    input[1].ki.dwFlags = KEYEVENTF_UNICODE | KEYEVENTF_KEYUP;

    SendInput(2, input, sizeof(INPUT));
}

void HandleUmlaut(wchar_t lower, wchar_t upper) {
    if (GetAsyncKeyState(VK_SHIFT) & 0x8000)
        SendUnicodeChar(upper);
    else
        SendUnicodeChar(lower);
}

// Keyboard hook procedure
LRESULT CALLBACK KeyboardProc(int nCode, WPARAM wParam, LPARAM lParam) {
    if (nCode >= 0 && wParam == WM_KEYDOWN) {
        KBDLLHOOKSTRUCT *kbd = (KBDLLHOOKSTRUCT *)lParam;

        // Detect Ctrl + Alt combination
        if ((GetAsyncKeyState(VK_CONTROL) & 0x8000) && (GetAsyncKeyState(VK_MENU) & 0x8000)) {
            switch (kbd->vkCode) {
                case 'A': HandleUmlaut(L'ä', L'Ä'); return 1;
                case 'O': HandleUmlaut(L'ö', L'Ö'); return 1;
                case 'U': HandleUmlaut(L'ü', L'Ü'); return 1;
                case 'S': HandleUmlaut(L'ß', L'ẞ'); return 1;
            }
        }
    }
    return CallNextHookEx(hHook, nCode, wParam, lParam);
}

// Start the keyboard mapping hook
void StartKeymap() {
    hHook = SetWindowsHookEx(WH_KEYBOARD_LL, KeyboardProc, NULL, 0);
    if (!hHook) {
        MessageBoxW(NULL, L"Failed to install keyboard hook.", L"Error", MB_ICONERROR);
    }
}

// Stop the keyboard mapping hook
void StopKeymap() {
    if (hHook) {
        UnhookWindowsHookEx(hHook);
        hHook = NULL;
    }
}
