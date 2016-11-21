// (0) PUBLIC DOMAIN
// To the extent possible under law, the person who associated CC0 with this work
// has waived all copyright and related or neighboring rights to this work.

#r "System"
#r "System.Drawing"
#r "System.Windows.Forms"

open System
open System.IO
open System.Text
open System.Drawing
open System.Drawing.Imaging
open System.Threading
open System.Windows.Forms
open System.Runtime.InteropServices

// P/Invoke

#nowarn "9"

[<StructLayout(LayoutKind.Sequential)>]
type RECT =
    struct
        val mutable left  :int
        val mutable top   :int
        val mutable right :int
        val mutable bottom:int
        new(l, t, r, b) = { left = l; top = t; right = r; bottom = b }
        static member Empty = RECT(0, 0, 0, 0)
    end

[<StructLayout(LayoutKind.Sequential)>]
type PAINTSTRUCT =
    struct
        val mutable hdc        :nativeint
        val mutable fErase     :bool
        val mutable rcPaint    :RECT
        val mutable fRestore   :bool
        val mutable fIncUpdate :bool
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 32)>]
        val mutable rgbReserved:byte[]
        new (h, fe, r, fr, fi, rgb) =
            { hdc = h; fErase = fe; rcPaint = r; fRestore = fr; fIncUpdate = fi; rgbReserved = rgb }
        static member Empty =
            PAINTSTRUCT(nativeint 0, false, RECT.Empty, false, false, Array.zeroCreate<byte> 32)
    end

[<DllImport("user32.dll")>] extern nativeint GetDesktopWindow()
[<DllImport("user32.dll")>] extern nativeint WindowFromPoint(Point p)
[<DllImport("user32.dll")>] extern bool SetWindowPos(
    nativeint hWnd, nativeint hWndInsertAfter, int X, int Y, int cx, int cy, int uFlags)

let HWND_TOP = 0
let SWP_NOSIZE = 0x0001
let SWP_NOMOVE = 0x0002

[<DllImport("user32.dll")>] extern bool GetWindowRect(nativeint hWnd, RECT& lpRect)
[<DllImport("user32.dll")>] extern bool GetClientRect(nativeint hWnd, RECT& lpRect)
[<DllImport("user32.dll", CharSet = CharSet.Unicode)>] extern int GetClassName(
    nativeint hWnd, StringBuilder lpClassName, int nMaxCount)
[<DllImport("user32.dll")>] extern nativeint BeginPaint(nativeint hwnd, PAINTSTRUCT& lpPaint)
[<DllImport("user32.dll")>] extern bool EndPaint(nativeint hWnd, PAINTSTRUCT& lpPaint)
[<DllImport("user32.dll")>] extern nativeint GetAncestor(nativeint hwnd, int gaFlags)

let GA_ROOT = 2

[<DllImport("user32.dll")>] extern nativeint SendMessage(nativeint hWnd, int Msg, int wParam, int lParam)
[<DllImport("user32.dll")>] extern bool PostMessage(nativeint hWnd, int Msg, int wParam, int lParam)

let WM_MOUSEMOVE   = 0x0200
let WM_LBUTTONDOWN = 0x0201
let WM_LBUTTONUP   = 0x0202
let WM_ACTIVATE    = 0x0006
let WA_ACTIVE      = 1

[<DllImport("user32.dll")>] extern nativeint GetForegroundWindow()
[<DllImport("user32.dll")>] extern bool SetForegroundWindow(nativeint hWnd)
[<DllImport("user32.dll")>] extern int GetWindowThreadProcessId(nativeint hWnd, int[] lpdwProcessId)
[<DllImport("user32.dll")>] extern bool AttachThreadInput(int idAttach, int idAttachTo, bool fAttach)
[<DllImport("user32.dll")>] extern bool SystemParametersInfo(int uiAction, int uiParam, int& pvParam, int fWinIni)
[<DllImport("user32.dll")>] extern bool ScreenToClient(nativeint hWnd, Point& lpPoint)
[<DllImport("user32.dll")>] extern bool ClientToScreen(nativeint hWnd, Point& lpPoint)

let SPI_GETFOREGROUNDLOCKTIMEOUT = 0x2000
let SPI_SETFOREGROUNDLOCKTIMEOUT = 0x2001

[<DllImport("gdi32.dll")>] extern bool BitBlt(
    nativeint hdcDest, int nXDest, int nYDest, int nWidth, int nHeight,
    nativeint hdcSrc, int nXSrc, int nYSrc, int dwRop)

let SRCCOPY = 0x00cc0020

[<DllImport("msvcrt.dll", CallingConvention=CallingConvention.Cdecl)>]
extern int memcmp(nativeint b1, nativeint b2, unativeint count)

// Wrappers and Utilities

let getWindowRect hWnd =
    let mutable r = RECT.Empty
    if GetWindowRect(hWnd, &r) then
        Rectangle.FromLTRB(r.left, r.top, r.right, r.bottom)
    else
        Rectangle.Empty

let getClientRect hWnd =
    let mutable r = RECT.Empty
    if GetClientRect(hWnd, &r) then
        Rectangle.FromLTRB(r.left, r.top, r.right, r.bottom)
    else
        Rectangle.Empty

let getClientRectEx hWnd =
    let r1 = getWindowRect hWnd
    let r2 = getClientRect hWnd
    if r1 = Rectangle.Empty || r2 = Rectangle.Empty then r1 else
    let margin = (r1.Width - r2.Width) / 2
    let cx = r1.X + margin
    let cy = r1.Y + (r1.Height - r2.Height - margin)
    Rectangle(cx, cy, r2.Width, r2.Height)

let activateWindow hWnd =
    let fore = GetWindowThreadProcessId(GetForegroundWindow(), null)
    let target = GetWindowThreadProcessId(hWnd, null)
    ignore <| AttachThreadInput(target, fore, true)

    let mutable t1, t2 = 0, 0
    ignore <| SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, &t1, 0)
    ignore <| SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, &t2, 0)
    ignore <| SetForegroundWindow(hWnd)
    ignore <| SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, &t1, 0)

    ignore <| AttachThreadInput(target, fore, false)

let getClassName hWnd =
    let s = StringBuilder(256)
    if GetClassName(hWnd, s, s.Capacity) = 0 then null else s.ToString()

let captureWindow hWnd client =
    let desktop = GetDesktopWindow()
    let r = (if client then getClientRectEx else getWindowRect) hWnd
    let ret = new Bitmap(r.Width, r.Height)
    use g = Graphics.FromImage(ret)
    g.FillRectangle(Brushes.Black, 0, 0, r.Width, r.Height)
    let hdc1 = g.GetHdc()
    let mutable ps = PAINTSTRUCT.Empty
    let hdc2 = BeginPaint(desktop, &ps)
    ignore <| BitBlt(hdc1, 0, 0, r.Width, r.Height, hdc2, r.X, r.Y, SRCCOPY)
    ignore <| EndPaint(desktop, &ps)
    g.ReleaseHdc(hdc1)
    ret

let makeXY x y = (x &&& 0xffff) ||| ((y &&& 0xffff) <<< 16)
let getX lp =  lp         |> uint16 |> int16 |> int
let getY lp = (lp >>> 16) |> uint16 |> int16 |> int

let bmpcmp (bmp1:Bitmap) (bmp2:Bitmap) =
    if bmp1 = null || bmp2 = null || bmp1.Size <> bmp2.Size then false else
    let bd1 = bmp1.LockBits(Rectangle(Point.Empty, bmp1.Size), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
    let bd2 = bmp2.LockBits(Rectangle(Point.Empty, bmp2.Size), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
    let bs1 = (abs bd1.Stride) * bmp1.Height
    let bs2 = (abs bd2.Stride) * bmp2.Height
    let ret = bs1 = bs2 && memcmp(bd1.Scan0, bd2.Scan0, unativeint bs1) = 0
    bmp1.UnlockBits bd1
    bmp2.UnlockBits bd2
    ret

let clipbmp (bmp:Bitmap) (r:Rectangle) =
    let ret = new Bitmap(r.Width, r.Height, PixelFormat.Format32bppArgb)
    use g = Graphics.FromImage ret
    g.DrawImage(bmp, Rectangle(Point.Empty, r.Size), r, GraphicsUnit.Pixel)
    ret

let trimbmp (bmp:Bitmap) margin =
    let bd = bmp.LockBits(Rectangle(Point.Empty, bmp.Size), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
    let stride = (abs bd.Stride) >>> 2
    let bs = stride * bmp.Height
    let pxs = Array.zeroCreate<int> bs
    Marshal.Copy(bd.Scan0, pxs, 0, int pxs.Length)
    bmp.UnlockBits bd
    let c = pxs.[0]
    let x1, x2 =
        let rec loopy y idx =
            if y >= bmp.Height then true
            elif pxs.[idx] <> c then false
            else loopy (y + 1) (idx + stride)
        let rec loopx x =
            if x >= bmp.Width then bmp.Width
            elif not (loopy 0 x) then x else loopx (x + 1)
        let x1 = loopx 0
        let rec loopx x =
            if x <= x1 then x1 else
            if not (loopy 0 x) then x else loopx (x - 1)
        x1, loopx (bmp.Width - 1)
    let y1, y2 =
        let rec loopx x idx =
            if x >= x2 then true
            elif pxs.[idx] <> c then false
            else loopx (x + 1) (idx + 1)
        let rec loopy y =
            if y >= bmp.Height then bmp.Height else
            if not (loopx x1 (x1 + y * stride)) then y else loopy (y + 1)
        let y1 = loopy 0
        let rec loopy y =
            if y <= y1 then y1 else
            if not (loopx x1 (x1 + y * stride)) then y else loopy (y - 1)
        y1, loopy (bmp.Height - 1)
    let r = Rectangle(x1, y1, x2 - x1 + 1, y2 - y1 + 1)
    let ret = new Bitmap(
                r.Width + margin * 2, r.Height + margin * 2,
                PixelFormat.Format32bppArgb)
    use g = Graphics.FromImage ret
    g.Clear(Color.FromArgb c)
    g.DrawImage(bmp, Rectangle(margin, margin, r.Width, r.Height), r, GraphicsUnit.Pixel)
    ret

let drag hWnd x1 y1 x2 y2 =
    let lp1 = makeXY x1 y1
    ignore <| SendMessage(hWnd, WM_MOUSEMOVE, 0, lp1)
    ignore <| SendMessage(hWnd, WM_LBUTTONDOWN, 0, lp1)
    let lp2 = makeXY x2 y2
    ignore <| SendMessage(hWnd, WM_MOUSEMOVE, 1(*MK_LBUTTON*), lp2)
    ignore <| SendMessage(hWnd, WM_LBUTTONUP, 0, lp2)

// Main

[<EntryPoint;STAThread>] do
Application.SetCompatibleTextRenderingDefault true

let f2 = new Form(
            Bounds = Screen.PrimaryScreen.Bounds,
            BackColor = Color.White,
            TopMost = true,
            Opacity = 0.6,
            FormBorderStyle = FormBorderStyle.None,
            Cursor = Cursors.Cross)
f2.KeyDown.Add <| fun e ->
    if e.KeyCode = Keys.Escape then
        f2.DialogResult <- DialogResult.Cancel
        f2.Close()
let rb = new Control(Bounds = Rectangle.Empty, BackColor = Color.Blue)
f2.Controls.Add rb
let start = ref Point.Empty
f2.MouseDown.Add <| fun e ->
    if e.Button = MouseButtons.Left then
        rb.Bounds <- Rectangle(e.X, e.Y, 0, 0)
        rb.Visible <- true
        start := e.Location
f2.MouseMove.Add <| fun e ->
    if rb.Visible then
        let f x y = min x y, max x y
        let x1, x2 = f e.X (!start).X
        let y1, y2 = f e.Y (!start).Y
        rb.Bounds <- Rectangle.FromLTRB(x1, y1, x2, y2)
f2.MouseUp.Add <| fun e ->
    if rb.Visible then
        rb.Visible <- false
        if rb.Height >= 16 then
            f2.DialogResult <- DialogResult.OK

let f1 = new Form(
            ClientSize = Size(200, 50),
            TopMost = true)
let b1 = new Button(
            Text = "Start!",
            Bounds = Rectangle(60, 10, 80, 30))
f1.Controls.Add b1
let no = ref 0
b1.Click.Add <| fun _ ->
    f1.Visible <- false
    rb.Visible <- false
    let r = f2.ShowDialog()
    if r = DialogResult.OK then
        let mutable p = rb.Location
        let target = WindowFromPoint(p)
        if getClassName target <> "MozillaWindowClass" then () else
        let cn = getClassName target
        printfn "className: %s" cn
        ignore <| ScreenToClient(target, &p)
        ignore <| activateWindow(GetAncestor(target, GA_ROOT))
        let r = Rectangle(p, rb.Size)
        let list = new Collections.Generic.List<Bitmap>()
        let rec loop prev =
            Thread.Sleep 500
            let bmp1 = captureWindow target true
            let bmp2 = clipbmp bmp1 r
            bmp1.Dispose()
            if list.Count > 20 || bmpcmp prev bmp2 then bmp2.Dispose() else
            list.Add bmp2
            drag target r.X r.Bottom r.X r.Y
            loop bmp2
        loop null
        use bmpm = new Bitmap(r.Width, r.Height * list.Count, PixelFormat.Format32bppArgb)
        let g = Graphics.FromImage bmpm
        for i = 0 to list.Count - 1 do
            Thread.Sleep 500
            use b = list.[i]
            g.DrawImageUnscaled(b, 0, i * r.Height)
            drag target r.X r.Y r.X r.Bottom
        g.Dispose()
        use bmpt = trimbmp bmpm 16
        let dir = Path.GetDirectoryName Application.ExecutablePath
        let rec loop() =
            let f = Path.Combine(dir, sprintf "output-%04d.png" !no)
            no := !no + 1
            if File.Exists f then loop() else bmpt.Save f
        loop()
    f1.Visible <- true
    activateWindow f1.Handle
 
Application.EnableVisualStyles ()
Application.Run f1
