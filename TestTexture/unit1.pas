unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, MediaUtils, Setup, Math;

type TForm1 = class(TCommonForm)
private
  var VertexArray: TGLuint;
  var VertexBuffer: TGLuint;
  var IndexBuffer: TGLuint;
  var VertexShader: TGLuint;
  var PixelShader: TGLuint;
  var UniformWVP: TGLint;
  var Shader: TGLuint;
  var TextureTemp: TGLuint;
  var Texture: TGLuint;
  var UniformTex0: TGLint;
  var TaskLoadTexture: specialize TUTask<TGLuint>;
  procedure ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
  function TF_LoadTexture(const Args: array of const): TGLuint;
protected
  function RequestDebugContext: Boolean; override;
public
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
end;

var Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
begin
  case ImageFormat of
    uif_g8: begin Format := GL_RED; DataType := GL_UNSIGNED_BYTE; end;
    uif_g16: begin Format := GL_RED; DataType := GL_UNSIGNED_SHORT; end;
    uif_g8a8: begin Format := GL_RG; DataType := GL_UNSIGNED_BYTE; end;
    uif_g16a16: begin Format := GL_RG; DataType := GL_UNSIGNED_SHORT; end;
    uif_r8g8b8: begin Format := GL_RGB; DataType := GL_UNSIGNED_BYTE; end;
    uif_r16g16b16: begin Format := GL_RGB; DataType := GL_UNSIGNED_SHORT; end;
    uif_r8g8b8a8: begin Format := GL_RGBA; DataType := GL_UNSIGNED_BYTE; end;
    uif_r16g16b16a16: begin Format := GL_RGBA; DataType := GL_UNSIGNED_SHORT; end;
    uif_r32g32b32_f: begin Format := GL_RGB; DataType := GL_FLOAT; end;
    else begin Format := 0; DataType := 0; end;
  end;
end;

function TForm1.TF_LoadTexture(const Args: array of const): TGLuint;
  var f: String;
  var Image: TUImageDataShared;
  var TextureFormat, TextureType: TGLenum;
  var MipLevels: UInt32;
begin
  if Length(Args) < 1 then Exit(0);
  f := AnsiString(Args[0].VAnsiString);
  Image := ULoadImageData(f);
  if not Image.IsValid then Exit(0);
  MipLevels := Round(Math.Log2(UMax(Image.Ptr.Width, Image.Ptr.Height)));
  MakeCurrentShared;
  ImageFormatToGL(Image.Ptr.Format, TextureFormat, TextureType);
  glCreateTextures(GL_TEXTURE_2D, 1, @Result);
  glTextureParameteri(Result, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTextureParameteri(Result, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTextureParameteri(Result, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTextureParameteri(Result, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTextureStorage2D(Result, MipLevels, GL_RGB8, Image.Ptr.Width, Image.Ptr.Height);
  glTextureSubImage2D(
    Result, 0, 0, 0,
    Image.Ptr.Width, Image.Ptr.Width,
    TextureFormat, TextureType, Image.Ptr.Data
  );
  glGenerateTextureMipmap(Result);
  // old style texture creation requires binding to context
  //glGenTextures(1, @Result);
  //glBindTexture(GL_TEXTURE_2D, Result);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  //glTexImage2D(
  //  GL_TEXTURE_2D, 0, GL_RGB,
  //  Image.Ptr.Width, Image.Ptr.Height, 0,
  //  TextureFormat, TextureType, Image.Ptr.Data
  //);
  //glGenerateMipmap(GL_TEXTURE_2D);
  glFinish();
end;

function TForm1.RequestDebugContext: Boolean;
begin
  Result := True;
end;

procedure TForm1.Initialize;
  const Vertices: array[0..3] of packed record
    p: TUVec3;
    c: TUVec4;
    t: TUVec2;
  end = (
    (p: (-1, 1, 0); c: (1, 0, 0, 1); t: (0, 0)),
    (p: (1, 1, 0); c: (0, 1, 0, 1); t: (1, 0)),
    (p: (-1, -1, 0); c: (0, 0, 1, 1); t: (0, 1)),
    (p: (1, -1, 0); c: (1, 1, 0, 1); t: (1, 1))
  );
  const Indices: array[0..5] of TGLubyte = (
    0, 1, 3, 0, 3, 2
  );
  const TextureData: array[0..3] of array[0..2] of UInt8 = (
    (0, 0, 0), ($ff, $ff, $ff), ($ff, $ff, $ff), (0, 0, 0)
  );
  var ShaderSource: String;
  var Ptr: Pointer;
  var i: Integer;
  var ErrorBuffer: array[0..511] of AnsiChar;
begin
  glCreateVertexArrays(1, @VertexArray);
  glCreateBuffers(1, @VertexBuffer);
  glNamedBufferStorage(VertexBuffer, SizeOf(Vertices), @Vertices, 0);
  glCreateBuffers(1, @IndexBuffer);
  glNamedBufferStorage(IndexBuffer, SizeOf(Indices), @Indices, 0);
  glVertexArrayVertexBuffer(VertexArray, 0, VertexBuffer, 0, SizeOf(Vertices[0]));
  glVertexArrayElementBuffer(VertexArray, IndexBuffer);
  glEnableVertexArrayAttrib(VertexArray, 0);
  glVertexArrayAttribFormat(VertexArray, 0, 3, GL_FLOAT, GL_FALSE, 0);
  glVertexArrayAttribBinding(VertexArray, 0, 0);
  glEnableVertexArrayAttrib(VertexArray, 1);
  glVertexArrayAttribFormat(VertexArray, 1, 4, GL_FLOAT, GL_FALSE, 12);
  glVertexArrayAttribBinding(VertexArray, 1, 0);
  glEnableVertexArrayAttrib(VertexArray, 2);
  glVertexArrayAttribFormat(VertexArray, 2, 2, GL_FLOAT, GL_FALSE, 28);
  glVertexArrayAttribBinding(VertexArray, 2, 0);
  { old style buffer creation with context binding
  glGenVertexArrays(1, @VertexArray);
  glGenBuffers(1, @VertexBuffer);
  glGenBuffers(1, @IndexBuffer);
  glBindVertexArray(VertexArray);
  glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(Vertices), @Vertices, GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IndexBuffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, SizeOf(Indices), @Indices, GL_STATIC_DRAW);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(0));
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(12));
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, SizeOf(Vertices[0]), Pointer(28));
  glEnableVertexAttribArray(2);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  //}
  VertexShader := glCreateShader(GL_VERTEX_SHADER);
  ShaderSource := UFileToStr(LocalFile('shader_vs.txt'));
  Ptr := PAnsiChar(ShaderSource);
  glShaderSource(VertexShader, 1, @Ptr, nil);
  glCompileShader(VertexShader);
  glGetShaderiv(VertexShader, GL_COMPILE_STATUS, @i);
  if i = 0 then
  begin
    glGetShaderInfoLog(VertexShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  PixelShader := glCreateShader(GL_FRAGMENT_SHADER);
  ShaderSource := UFileToStr(LocalFile('shader_ps.txt'));
  Ptr := PAnsiChar(ShaderSource);
  glShaderSource(PixelShader, 1, @Ptr, nil);
  glCompileShader(PixelShader);
  glGetShaderiv(PixelShader, GL_COMPILE_STATUS, @i);
  if i = 0 then
  begin
    glGetShaderInfoLog(PixelShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  Shader := glCreateProgram();
  glAttachShader(Shader, VertexShader);
  glAttachShader(Shader, PixelShader);
  glLinkProgram(Shader);
  glGetProgramiv(Shader, GL_LINK_STATUS, @i);
  if i = 0 then
  begin
    glGetProgramInfoLog(Shader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  glDeleteShader(PixelShader);
  glDeleteShader(VertexShader);
  UniformWVP := glGetUniformLocation(Shader, PGLchar(PAnsiChar('WVP')));
  UniformTex0 := glGetUniformLocation(Shader, PGLchar(PAnsiChar('tex0')));
  // create a temporary texture while the main one is loading
  glCreateTextures(GL_TEXTURE_2D, 1, @TextureTemp);
  glTextureParameteri(TextureTemp, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTextureParameteri(TextureTemp, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTextureParameteri(TextureTemp, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTextureParameteri(TextureTemp, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTextureStorage2D(TextureTemp, 1, GL_RGB8, 2, 2);
  glTextureSubImage2D(
    TextureTemp, 0, 0, 0,
    2, 2, GL_RGB, GL_UNSIGNED_BYTE, @TextureData
  );
  //glGenTextures(1, @TextureTemp);
  //glBindTexture(GL_TEXTURE_2D, TextureTemp);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  //glTexImage2D(
  //  GL_TEXTURE_2D, 0, GL_RGB,
  //  2, 2, 0,
  //  GL_RGB, GL_UNSIGNED_BYTE,
  //  @TextureData
  //);
  Texture := 0;
  TaskLoadTexture := TaskLoadTexture.StartTask(@TF_LoadTexture, [AssetsFile('crate_c.png')]);
  Caption := 'PasOpenGL Loading...';
end;

procedure TForm1.Finalize;
begin
  if (Texture > 0) then glDeleteTextures(1, @Texture);
  glDeleteTextures(1, @TextureTemp);
  glDeleteProgram(Shader);
  glDeleteBuffers(1, @VertexBuffer);
  glDeleteBuffers(1, @IndexBuffer);
  glDeleteVertexArrays(1, @VertexArray);
end;

procedure TForm1.Tick;
  function GetTexture: TGLuint;
  begin
    if Texture > 0 then Exit(Texture);
    Result := TextureTemp;
  end;
  var W, V, P, WVP: TUMat;
begin
  if TaskLoadTexture.IsComplete then
  begin
    Texture := TaskLoadTexture.TaskResult;
    TaskLoadTexture.Reset;
    Caption := 'PasOpenGL';
  end;
  W := TUMat.RotationY(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  v := TUMat.View(TUVec3.Make(0, 1.5, -2), TUVec3.Zero, TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.5, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;
  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0.4, 1, 0.8, 1);
  //glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT);// or GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(VertexArray);
  glUseProgram(Shader);
  glUniformMatrix4fv(UniformWVP, 1, GL_TRUE, @WVP);
  glBindTextureUnit(0, GetTexture);
  //glActiveTexture(GL_TEXTURE0);
  //glBindTexture(GL_TEXTURE_2D, GetTexture);
  glUniform1i(UniformTex0, 0);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_BYTE, nil);
end;

end.

