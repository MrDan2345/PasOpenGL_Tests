unit Setup;

interface

{$macro on}

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, PasOpenGL,
{$if defined(WINDOWS)}
  Windows,
{$elseif defined(LINUX)}
  X, XLib, XUtil,
  gdk2x, gtk2,
{$endif}
  CommonUtils;

{$if defined(WINDOWS)}
  {$define calldecl := stdcall}
{$else}
  {$define calldecl := cdecl}
{$endif}

type TCommonForm = class(TForm)
  procedure FormActivate(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure OnTimer(Sender: TObject);
private
  var Timer: TTimer;
{$if defined(WINDOWS)}
  var RenderContext: HGLRC;
  var DeviceContext: HDC;
{$elseif defined(LINUX)}
  var Display: PDisplay;
  var Context: TGLXContext;
  function NativeHandle: TWindow;
{$endif}
{$if defined(WINDOWS)}
  procedure WinInitializeOpenGL;
  procedure WinFinalizeOpenGL;
{$elseif defined(LINUX)}
  procedure LinuxInitializeOpenGL;
  procedure LinuxFinalizeOpenGL;
{$endif}
  procedure DumpCallStack;
protected
  function RequestDebugContext: Boolean; virtual;
  procedure DebugMessage(
    const MsgSource, MsgType, MsgSeverity: GLenum;
    const MsgId: UInt32; const MsgStr: String;
    const SourceStr, TypeStr, SeverityStr: String
  ); virtual;
public
  function IsDebugContext: Boolean;
  procedure InitializeOpenGL;
  procedure FinalizeOpenGL;
  procedure PrintInfo;
  procedure MakeCurrentPrimary;
  procedure MakeCurrentShared;
  procedure Initialize; virtual;
  procedure Finalize; virtual;
  procedure Tick; virtual;
  function LocalFile(const FileName: String): String;
  function AssetsFile(const FileName: String): String;
  constructor Create(TheOwner: TComponent); override;
  destructor Destroy; override;
end;

implementation

procedure DebugOutout(
  source: GLenum;
  _type: GLenum;
  id: GLuint;
  severity: GLenum;
  length: GLsizei;
  const _message: PGLchar;
  const userParam: Pointer
); calldecl;
  var SourceStr, TypeStr, SeverityStr, MsgStr: String;
begin
  case source of
    GL_DEBUG_SOURCE_API: SourceStr := 'API';
    GL_DEBUG_SOURCE_WINDOW_SYSTEM: SourceStr := 'Window System';
    GL_DEBUG_SOURCE_SHADER_COMPILER: SourceStr := 'Shader Compiler';
    GL_DEBUG_SOURCE_THIRD_PARTY: SourceStr := 'Third Party';
    GL_DEBUG_SOURCE_APPLICATION: SourceStr := 'Application';
    GL_DEBUG_SOURCE_OTHER: SourceStr := 'Other';
  end;
  case _type of
    GL_DEBUG_TYPE_ERROR: TypeStr := 'Error';
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: TypeStr := 'Deprecated Behaviour';
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR: TypeStr := 'Undefined Behaviour';
    GL_DEBUG_TYPE_PORTABILITY: TypeStr := 'Portability';
    GL_DEBUG_TYPE_PERFORMANCE: TypeStr := 'Performance';
    GL_DEBUG_TYPE_MARKER: TypeStr := 'Marker';
    GL_DEBUG_TYPE_PUSH_GROUP: TypeStr := 'Push Group';
    GL_DEBUG_TYPE_POP_GROUP: TypeStr := 'Pop Group';
    GL_DEBUG_TYPE_OTHER: TypeStr := 'Other';
  end;
  case severity of
    GL_DEBUG_SEVERITY_HIGH: SeverityStr := 'High';
    GL_DEBUG_SEVERITY_MEDIUM: SeverityStr := 'Medium';
    GL_DEBUG_SEVERITY_LOW: SeverityStr := 'Low';
    GL_DEBUG_SEVERITY_NOTIFICATION: SeverityStr := 'Notification';
  end;
  MsgStr := '';
  SetLength(MsgStr, length);
  Move(_message^, MsgStr[1], length);
  TCommonForm(userParam).DebugMessage(
    source, _type, severity, id, MsgStr,
    SourceStr, TypeStr, SeverityStr
  );
end;

procedure TCommonForm.FormActivate(Sender: TObject);
begin
  if Timer.Enabled then Exit;;
  InitializeOpenGL;
  PrintInfo;
  if IsDebugContext then
  begin
    glEnable(GL_DEBUG_OUTPUT);
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
    glDebugMessageCallback(@DebugOutout, Self);
    glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil, GL_TRUE);
    glSetDebugMode(True);
  end;
  Initialize;
  Timer.Enabled := True;
end;

procedure TCommonForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer.Enabled := False;
  Finalize;
  FinalizeOpenGL;
end;

procedure TCommonForm.OnTimer(Sender: TObject);
begin
  Tick;
{$if defined(WINDOWS)}
  SwapBuffers(DeviceContext);
{$elseif defined(LINUX)}
  glXSwapBuffers(Display, NativeHandle);
{$endif}
end;

{$if defined(WINDOWS)}
procedure TCommonForm.WinInitializeOpenGL;
  var pfd: TPixelFormatDescriptor;
  var pf: Integer;
  var pfn: GLuint;
  var FormatAttribs: TGLAttribs;
  var ContextAttribs: TGLAttribs;
begin
  DeviceContext := GetDC(Handle);
  FormatAttribs[WGL_DRAW_TO_WINDOW_ARB] := GL_TRUE;
  FormatAttribs[WGL_SUPPORT_OPENGL_ARB] := GL_TRUE;
  FormatAttribs[WGL_ACCELERATION_ARB] := WGL_FULL_ACCELERATION_ARB;
  FormatAttribs[WGL_COLOR_BITS_ARB] := 24;
  FormatAttribs[WGL_ALPHA_BITS_ARB] := 8;
  FormatAttribs[WGL_DEPTH_BITS_ARB] := 24;
  FormatAttribs[WGL_STENCIL_BITS_ARB] := 8;
  FormatAttribs[WGL_DOUBLE_BUFFER_ARB] := GL_TRUE;
  FormatAttribs[WGL_SAMPLE_BUFFERS_ARB] := GL_TRUE;
  FormatAttribs[WGL_SAMPLES_ARB] := 4;
  pfn := 1; pf := 0;
  if not wglChoosePixelFormatARB(DeviceContext, FormatAttribs.Data, nil, 1, @pf, @pfn) then
  begin
    WriteLn(glGetError);
    pfn := 0;
  end;
  if (pfn = 0) then
  begin
    UClear(pfd, SizeOf(pfd));
    pfd.nSize := SizeOf(pfd);
    pfd.nVersion := 1;
    pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    pfd.iPixelType := PFD_TYPE_RGBA;
    pfd.cColorBits := 32;
    pfd.cAlphaBits := 8;
    pfd.cDepthBits := 16;
    pfd.iLayerType := PFD_MAIN_PLANE;
    pf := ChoosePixelFormat(DeviceContext, @pfd);
  end;
  SetPixelFormat(DeviceContext, pf, @pfd);
  ContextAttribs[WGL_CONTEXT_MAJOR_VERSION_ARB] := 4;
  ContextAttribs[WGL_CONTEXT_MINOR_VERSION_ARB] := 5;
  if RequestDebugContext then
  begin
    ContextAttribs[WGL_CONTEXT_FLAGS_ARB] := WGL_CONTEXT_DEBUG_BIT_ARB;
  end;
  //ContextAttribs[WGL_CONTEXT_PROFILE_MASK_ARB] := WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
  //ContextAttribs[WGL_CONTEXT_PROFILE_MASK_ARB] := WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
  ContextAttribs[WGL_CONTEXT_PROFILE_MASK_ARB] := WGL_CONTEXT_CORE_PROFILE_BIT_ARB;
  RenderContext := wglCreateContextAttribsARB(DeviceContext, glSharedContext, ContextAttribs.Data);
  if RenderContext = 0 then
  begin
    WriteLn(glGetError);
  end;
  wglMakeCurrent(DeviceContext, RenderContext);
end;

procedure TCommonForm.WinFinalizeOpenGL;
begin
  wglDeleteContext(RenderContext);
  ReleaseDC(Handle, DeviceContext);
end;

{$elseif defined(LINUX)}
function TCommonForm.NativeHandle: TWindow;
  function AsPtr(const Address: PtrUInt): Pointer;
    var Ptr: Pointer absolute Address;
  begin
    Result := Ptr;
  end;
  var Widget: PGtkWidget;
begin
  Widget := PGtkWidget(AsPtr(Handle));
  if not Assigned(Widget) then Exit(0);
  if not Assigned(Widget^.window) then Exit(0);
  Result := GDK_WINDOW_XWINDOW(Widget^.window);
end;

procedure TCommonForm.LinuxInitializeOpenGL;
  type TGLXFBConfigArr = array[UInt16] of TGLXFBConfig;
  type PGLXFBConfigArr = ^TGLXFBConfigArr;
  var VisualAttribs: TGLAttribs;
  var ContextAttribs: TGLAttribs;
  var Configs: PGLXFBConfigArr;
  var ConfigCount: Int32;
begin
  Display := XOpenDisplay(nil);
  VisualAttribs[GLX_X_RENDERABLE] := GL_TRUE;
  VisualAttribs[GLX_DRAWABLE_TYPE] := GLX_WINDOW_BIT;
  VisualAttribs[GLX_RENDER_TYPE] := GLX_RGBA_BIT;
  VisualAttribs[GLX_X_VISUAL_TYPE] := GLX_TRUE_COLOR;
  VisualAttribs[GLX_RED_SIZE] := 8;
  VisualAttribs[GLX_GREEN_SIZE] := 8;
  VisualAttribs[GLX_BLUE_SIZE] := 8;
  //VisualAttribs[GLX_ALPHA_SIZE] := 8;
  //VisualAttribs[GLX_DEPTH_SIZE] := 0;
  //VisualAttribs[GLX_STENCIL_SIZE] := 0;
  VisualAttribs[GLX_DOUBLEBUFFER] := GL_TRUE;
  //VisualAttribs[GLX_SAMPLE_BUFFERS] := 1;
  //VisualAttribs[GLX_SAMPLES] := 4;
  Configs := PGLXFBConfigArr(glXChooseFBConfig(Display, DefaultScreen(Display), VisualAttribs.Data, @ConfigCount));
  if not Assigned(Configs) then
  begin
    WriteLn(glGetError);
    Exit;
  end;
  //Can query config attribs to find the most suitable one
  //glXGetFBConfigAttrib
  ContextAttribs[GLX_CONTEXT_MAJOR_VERSION_ARB] := 4;
  ContextAttribs[GLX_CONTEXT_MINOR_VERSION_ARB] := 5;
  if RequestDebugContext then
  begin
    ContextAttribs[GLX_CONTEXT_FLAGS_ARB] := GLX_CONTEXT_DEBUG_BIT_ARB;
  end;
  //ContextAttribs[GLX_CONTEXT_PROFILE_MASK_ARB] := GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB;
  //ContextAttribs[GLX_CONTEXT_PROFILE_MASK_ARB] := GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB;
  ContextAttribs[GLX_CONTEXT_PROFILE_MASK_ARB] := GLX_CONTEXT_CORE_PROFILE_BIT_ARB;
  Context := glXCreateContextAttribsARB(Display, Configs^[0], glSharedContext, GL_TRUE, ContextAttribs.Data);
  //Context := glXCreateContext(Display, VisualInfo, glSharedContext, GL_TRUE);
  XFree(Configs);
  glXMakeCurrent(Display, NativeHandle, Context);
end;

procedure TCommonForm.LinuxFinalizeOpenGL;
begin
  glXDestroyContext(Display, Context);
  XCloseDisplay(Display);
end;
{$endif}

procedure TCommonForm.DumpCallStack;
  var CurFrame, PrevFrame: Pointer;
  var CallerAddress: Pointer;
  var CurDepth: UInt32;
  const MaxDepth = 20;
begin
  CurDepth := 0;
  CurFrame := glDebugFrame;
  repeat
     CallerAddress := get_caller_addr(CurFrame);
     PrevFrame := CurFrame;
     CurFrame := get_caller_frame(CurFrame);
     if CurFrame <= PrevFrame then Break;
     if not Assigned(CallerAddress) then Break;
     WriteLn(BackTraceStrFunc(CallerAddress));
     Inc(CurDepth);
     if CurDepth >= MaxDepth then Break;
  until not Assigned(CurFrame);
end;

function TCommonForm.RequestDebugContext: Boolean;
begin
  Result := False;
end;

procedure TCommonForm.DebugMessage(
  const MsgSource, MsgType, MsgSeverity: GLenum;
  const MsgId: UInt32; const MsgStr: String;
  const SourceStr, TypeStr, SeverityStr: String
);
  const MessagesToIgnore: array of UInt32 = (
    $00020071 // GL_STATIC_DRAW uses video memory
  );
  var MessageToIgnore: UInt32;
begin
  // ignore insignificant notifications
  for MessageToIgnore in MessagesToIgnore do
  if MsgId = MessageToIgnore then Exit;
  WriteLn('Debug Message (Source: ', SourceStr, '; Type: ', TypeStr, '; Severity: ', SeverityStr, '):');
  WriteLn('Id = ', IntToHex(MsgId));
  WriteLn('Message = ', MsgStr);
  if MsgType = GL_DEBUG_TYPE_ERROR then
  begin
    DumpCallStack;
    // raise am exception at the point of gl function call
    raise Exception.Create(MsgStr) at get_caller_addr(glDebugFrame), get_caller_frame(glDebugFrame);
  end;
end;

function TCommonForm.IsDebugContext: Boolean;
  var Flags: Int32;
begin
  glGetIntegerv(GL_CONTEXT_FLAGS, @Flags);
  Result := Flags and GL_CONTEXT_FLAG_DEBUG_BIT > 0;
end;

procedure TCommonForm.InitializeOpenGL;
begin
  {$if defined(WINDOWS)}
  WinInitializeOpenGL;
  {$elseif defined(LINUX)}
  LinuxInitializeOpenGL;
  {$endif}
end;

procedure TCommonForm.FinalizeOpenGL;
begin
  {$if defined(WINDOWS)}
  WinFinalizeOpenGL;
  {$elseif defined(LINUX)}
  LinuxFinalizeOpenGL;
  {$endif}
end;

procedure TCommonForm.PrintInfo;
  var s: String;
  var i, n: TGLint;
begin
  s := PAnsiChar(glGetString(GL_VERSION));
  WriteLn('OpenGL Version: ', s);
  glGetIntegerv(GL_MAJOR_VERSION, @i);
  s := IntToStr(i);
  glGetIntegerv(GL_MINOR_VERSION, @i);
  s += '.' + IntToStr(i);
  WriteLn(s);
  s := PAnsiChar(glGetString(GL_VENDOR));
  WriteLn('Vendor: ', s);
  s := PAnsiChar(glGetString(GL_RENDERER));
  WriteLn('Renderer: ', s);
  glGetIntegerv(GL_NUM_EXTENSIONS, @n);
  for i := 0 to n - 1 do
  begin
    s := PAnsiChar(glGetStringi(GL_EXTENSIONS, i));
    WriteLn(s);
  end;
end;

procedure TCommonForm.MakeCurrentPrimary;
begin
{$if defined(WINDOWS)}
  wglMakeCurrent(DeviceContext, RenderContext);
{$elseif defined(LINUX)}
  glXMakeCurrent(Display, NativeHandle, Context);
{$endif}
end;

procedure TCommonForm.MakeCurrentShared;
begin
{$if defined(WINDOWS)}
  wglMakeCurrent(glSharedDC, glSharedContext);
{$elseif defined(LINUX)}
  glXMakeCurrent(Display, glSharedWindow, glSharedContext);
{$endif}
end;

procedure TCommonForm.Initialize;
begin
end;

procedure TCommonForm.Finalize;
begin
end;

procedure TCommonForm.Tick;
begin
end;

function TCommonForm.LocalFile(const FileName: String): String;
begin
  Result := ExpandFileName(ExtractFileDir(ParamStr(0)) + '/../' + FileName);
end;

function TCommonForm.AssetsFile(const FileName: String): String;
begin
  Result := ExpandFileName(ExtractFileDir(ParamStr(0)) + '/../../Assets/' + FileName);
end;

constructor TCommonForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Timer := TTimer.Create(Self);
  Timer.Enabled := False;
  Timer.OnTimer := @OnTimer;
  Timer.Interval := 1;
end;

destructor TCommonForm.Destroy;
begin
  inherited Destroy;
end;

end.
