#include "gui.h"
#include "keymap.h"
#include "config.h"

HWND hButtonToggle, hStatusText, hIssueLink;
BOOL isKeymapActive = FALSE; // Tracks key mapping status

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

// Window procedure for handling UI events
LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_CREATE:
        // Status display text
        hStatusText = CreateWindowW(L"STATIC", L"Status: OFF", WS_VISIBLE | WS_CHILD,
                                    50, 20, 200, 20, hwnd, NULL,
                                    (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);

        // Toggle button for enabling/disabling key mapping
        hButtonToggle = CreateWindowW(L"BUTTON", L"Turn Keymap On", WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON,
                                      50, 50, 200, 30, hwnd, (HMENU)ID_BUTTON_TOGGLE,
                                      (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);

        // Issue report link (clickable text)
        hIssueLink = CreateWindowW(L"STATIC", L"🔗 Report an issue", WS_VISIBLE | WS_CHILD | SS_NOTIFY,
                                   50, 90, 200, 20, hwnd, (HMENU)ID_LINK_ISSUE,
                                   (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);

        // Set font and text color (blue with underline)
        SendMessageW(hIssueLink, WM_SETFONT, (WPARAM)GetStockObject(DEFAULT_GUI_FONT), TRUE);
        SetTextColor(GetDC(hIssueLink), RGB(0, 0, 255));

        break;

    case WM_COMMAND:
        switch (LOWORD(wParam))
        {
        case ID_BUTTON_TOGGLE:
            if (isKeymapActive)
            {
                StopKeymap();
            }
            else
            {
                StartKeymap();
            }
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

// Initializes the GUI and creates the main application window
void InitGUI(HINSTANCE hInstance, int nCmdShow)
{
    WNDCLASSW wc = {0};
    wc.lpfnWndProc = WindowProc;
    wc.hInstance = hInstance;
    wc.lpszClassName = L"KeymapGUI";

    RegisterClassW(&wc);

    HWND hwnd = CreateWindowExW(0, L"KeymapGUI", L"Keymap Utility",
                                WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, 300, 180,
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
