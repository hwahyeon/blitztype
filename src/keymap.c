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

// Keyboard hook procedure
LRESULT CALLBACK KeyboardProc(int nCode, WPARAM wParam, LPARAM lParam) {
    if (nCode >= 0 && wParam == WM_KEYDOWN) {
        KBDLLHOOKSTRUCT *kbd = (KBDLLHOOKSTRUCT *)lParam;

        // Detect Ctrl + Alt combination
        if ((GetAsyncKeyState(VK_CONTROL) & 0x8000) && (GetAsyncKeyState(VK_MENU) & 0x8000)) {
            switch (kbd->vkCode) {
                case 'A': SendUnicodeChar(GetAsyncKeyState(VK_SHIFT) & 0x8000 ? L'Ä' : L'ä'); return 1;
                case 'O': SendUnicodeChar(GetAsyncKeyState(VK_SHIFT) & 0x8000 ? L'Ö' : L'ö'); return 1;
                case 'U': SendUnicodeChar(GetAsyncKeyState(VK_SHIFT) & 0x8000 ? L'Ü' : L'ü'); return 1;
                case 'S': SendUnicodeChar(GetAsyncKeyState(VK_SHIFT) & 0x8000 ? L'ẞ' : L'ß'); return 1;
            }
        }
    }
    return CallNextHookEx(hHook, nCode, wParam, lParam);
}

// Start the keyboard mapping hook
void StartKeymap() {
    hHook = SetWindowsHookEx(WH_KEYBOARD_LL, KeyboardProc, NULL, 0);
}

// Stop the keyboard mapping hook
void StopKeymap() {
    UnhookWindowsHookEx(hHook);
}
