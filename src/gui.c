#include "gui.h"
#include "keymap.h"
#include "config.h"

HWND hButtonToggle, hStatusText, hIssueLink, hInstructionText;
BOOL isKeymapActive = FALSE;

// Opens the issue report page
void OpenIssuePage()
{
    ShellExecuteW(NULL, L"open", ISSUE_REPORT_URL, NULL, NULL, SW_SHOWNORMAL);
}

// Updates the UI to reflect the current key mapping status
void UpdateUI()
{
    SetWindowTextW(hButtonToggle, isKeymapActive ? L"Turn Keymap Off" : L"Turn Keymap On");
    SetWindowTextW(hStatusText, isKeymapActive ? L"Status: ON" : L"Status: OFF");
}

// Handles UI events
LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_CREATE:
    {
        HINSTANCE hInst = (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE);

        int centerX = 125;      // (450 - 200) / 2
        int narrowCenterX = 75; // (450 - 300) / 2

        // Status display (centered text)
        hStatusText = CreateWindowW(L"STATIC", L"Status: OFF", WS_VISIBLE | WS_CHILD | SS_CENTER,
                                    centerX, 20, 200, 20, hwnd, NULL, hInst, NULL);

        // Toggle keymap button
        hButtonToggle = CreateWindowW(L"BUTTON", L"Turn Keymap On", WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON,
                                      centerX, 50, 200, 30, hwnd, (HMENU)ID_BUTTON_TOGGLE, hInst, NULL);

        // Instruction (centered placement, fixed-width font)
        hInstructionText = CreateWindowW(
            L"STATIC",
            L"Key Combo              | Output\n"
            L"-----------------------+--------\n"
            L"Ctrl + Alt + (Shift)+A | ä / Ä\n"
            L"Ctrl + Alt + (Shift)+O | ö / Ö\n"
            L"Ctrl + Alt + (Shift)+U | ü / Ü\n"
            L"Ctrl + Alt + (Shift)+S | ß / ẞ\n"
            L"Ctrl + Alt + [         | „\n"
            L"Ctrl + Alt + ]         | “",
            WS_VISIBLE | WS_CHILD | SS_LEFT,
            narrowCenterX, 90, 300, 130, hwnd, NULL, hInst, NULL);

        HFONT hMonoFont = CreateFontW(
            16, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE,
            DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
            DEFAULT_QUALITY, FIXED_PITCH | FF_MODERN, L"Consolas");
        SendMessageW(hInstructionText, WM_SETFONT, (WPARAM)hMonoFont, TRUE);

        // Issue link (centered text)
        hIssueLink = CreateWindowW(L"STATIC", L"🔗 Report an issue", WS_VISIBLE | WS_CHILD | SS_NOTIFY | SS_CENTER,
                                   centerX, 230, 200, 20, hwnd, (HMENU)ID_LINK_ISSUE, hInst, NULL);

        SendMessageW(hIssueLink, WM_SETFONT, (WPARAM)GetStockObject(DEFAULT_GUI_FONT), TRUE);
        SetTextColor(GetDC(hIssueLink), RGB(0, 0, 255));
        break;
    }

    case WM_COMMAND:
        switch (LOWORD(wParam))
        {
        case ID_BUTTON_TOGGLE:
            if (isKeymapActive)
                StopKeymap();
            else
                StartKeymap();
            isKeymapActive = !isKeymapActive;
            UpdateUI();
            break;
        case ID_LINK_ISSUE:
            OpenIssuePage();
            break;
        }
        break;

    case WM_CLOSE:
        StopKeymap();
        DestroyWindow(hwnd);
        break;

    case WM_DESTROY:
        PostQuitMessage(0);
        break;

    default:
        return DefWindowProcW(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}

// Initializes the main window and enters message loop
void InitGUI(HINSTANCE hInstance, int nCmdShow)
{
    WNDCLASSW wc = {0};
    wc.lpfnWndProc = WindowProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = L"KeymapGUI";
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1); // optional: use COLOR_BTNFACE for gray

    RegisterClassW(&wc);

    HWND hwnd = CreateWindowExW(0, L"KeymapGUI", L"Keymap Utility",
                                WS_OVERLAPPEDWINDOW & ~WS_MAXIMIZEBOX & ~WS_THICKFRAME,
                                CW_USEDEFAULT, CW_USEDEFAULT, 450, 320,
                                NULL, NULL, hInstance, NULL);

    if (!hwnd)
        return;

    ShowWindow(hwnd, nCmdShow);

    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
}
