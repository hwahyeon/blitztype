#include "gui.h"
#include "keymap.h"
#include "config.h"

HWND hButtonToggle, hStatusText, hIssueLink, hInstructions;
BOOL isKeymapActive = FALSE;

HBRUSH hBackgroundBrush, hButtonBrush;
HFONT hSmallFont;

// Open the issue report page
void OpenIssuePage()
{
    ShellExecuteW(NULL, L"open", ISSUE_REPORT_URL, NULL, NULL, SW_SHOWNORMAL);
}

// Update UI text
void UpdateUI()
{
    SetWindowTextW(hButtonToggle, isKeymapActive ? L"Turn Keymap Off" : L"Turn Keymap On");
    SetWindowTextW(hStatusText, isKeymapActive ? L"Status: ON" : L"Status: OFF");
}

// Center X calculation
int CenterX(int parentWidth, int controlWidth)
{
    return (parentWidth - controlWidth) / 2;
}

// Window procedure
LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_CREATE:
    {
        hBackgroundBrush = CreateSolidBrush(RGB(24, 44, 85));
        hButtonBrush = CreateSolidBrush(RGB(40, 60, 100));

        LOGFONT lf = {0};
        lf.lfHeight = -14;
        wcscpy(lf.lfFaceName, L"Consolas");
        hSmallFont = CreateFontIndirectW(&lf);

        RECT rc;
        GetClientRect(hwnd, &rc);

        hStatusText = CreateWindowW(L"STATIC", L"Status: OFF",
                                    WS_VISIBLE | WS_CHILD | SS_CENTER,
                                    CenterX(rc.right, 200), 20, 200, 20,
                                    hwnd, NULL, (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);

        hButtonToggle = CreateWindowW(L"BUTTON", L"Turn Keymap On",
                                      WS_VISIBLE | WS_CHILD | BS_OWNERDRAW,
                                      CenterX(rc.right, 200), 50, 200, 30,
                                      hwnd, (HMENU)ID_BUTTON_TOGGLE,
                                      (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);

        const wchar_t *instructionText =
            L"|  Key Combo                   |  Output   |\n"
            L"|------------------------------+-----------|\n"
            L"|  Ctrl + Alt + (Shift) + A    |  ä / Ä    |\n"
            L"|  Ctrl + Alt + (Shift) + O    |  ö / Ö    |\n"
            L"|  Ctrl + Alt + (Shift) + U    |  ü / Ü    |\n"
            L"|  Ctrl + Alt + (Shift) + S    |  ß / ẞ    |\n"
            L"|  Ctrl + Alt + [              |  „        |\n"
            L"|  Ctrl + Alt + ]              |  “        |";

        hInstructions = CreateWindowW(L"STATIC", instructionText,
                                      WS_VISIBLE | WS_CHILD | SS_LEFT,
                                      CenterX(rc.right, 360), 95, 360, 140,
                                      hwnd, NULL, (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);

        hIssueLink = CreateWindowW(L"STATIC", L"🔗 Report an issue",
                                   WS_VISIBLE | WS_CHILD | SS_NOTIFY | SS_CENTER,
                                   CenterX(rc.right, 200), 245, 200, 20,
                                   hwnd, (HMENU)ID_LINK_ISSUE,
                                   (HINSTANCE)GetWindowLongPtr(hwnd, GWLP_HINSTANCE), NULL);

        HFONT defaultFont = (HFONT)GetStockObject(DEFAULT_GUI_FONT);
        SendMessageW(hStatusText, WM_SETFONT, (WPARAM)defaultFont, TRUE);
        SendMessageW(hButtonToggle, WM_SETFONT, (WPARAM)defaultFont, TRUE);
        SendMessageW(hInstructions, WM_SETFONT, (WPARAM)hSmallFont, TRUE);
        SendMessageW(hIssueLink, WM_SETFONT, (WPARAM)defaultFont, TRUE);
        break;
    }

    case WM_DRAWITEM:
    {
        LPDRAWITEMSTRUCT lpDraw = (LPDRAWITEMSTRUCT)lParam;
        SetBkColor(lpDraw->hDC, RGB(40, 60, 100));
        SetTextColor(lpDraw->hDC, RGB(220, 220, 220));
        FillRect(lpDraw->hDC, &lpDraw->rcItem, hButtonBrush);
        DrawTextW(lpDraw->hDC, L"Turn Keymap On", -1, &lpDraw->rcItem,
                  DT_CENTER | DT_VCENTER | DT_SINGLELINE);
        return TRUE;
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
            InvalidateRect(hButtonToggle, NULL, TRUE); // redraw
            break;
        case ID_LINK_ISSUE:
            OpenIssuePage();
            break;
        }
        break;

    case WM_CTLCOLORSTATIC:
    case WM_CTLCOLORBTN:
    {
        HDC hdc = (HDC)wParam;
        SetTextColor(hdc, RGB(220, 220, 220));
        SetBkMode(hdc, TRANSPARENT);
        return (INT_PTR)hBackgroundBrush;
    }

    case WM_CLOSE:
        StopKeymap();
        DestroyWindow(hwnd);
        break;

    case WM_DESTROY:
        PostQuitMessage(0);
        break;

    case WM_PAINT:
    {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint(hwnd, &ps);
        FillRect(hdc, &ps.rcPaint, hBackgroundBrush);
        EndPaint(hwnd, &ps);
        break;
    }

    default:
        return DefWindowProcW(hwnd, uMsg, wParam, lParam);
    }

    return 0;
}

// Entry point
void InitGUI(HINSTANCE hInstance, int nCmdShow)
{
    hBackgroundBrush = CreateSolidBrush(RGB(24, 44, 85));
    WNDCLASSW wc = {0};
    wc.lpfnWndProc = WindowProc;
    wc.hInstance = hInstance;
    wc.hbrBackground = NULL;
    wc.lpszClassName = L"KeymapGUI";

    RegisterClassW(&wc);

    HWND hwnd = CreateWindowExW(0, L"KeymapGUI", L"Quick Umlaut",
                                WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                                460, 340, NULL, NULL, hInstance, NULL);

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
