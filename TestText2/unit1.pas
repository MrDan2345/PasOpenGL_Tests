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
  var VertexShader: TGLuint;
  var PixelShader: TGLuint;
  var Shader: TGLuint;
  var UniformLines: TGLint;
  var UniformLineCount: TGLint;
  var UniformCurves: TGLint;
  var UniformCurveCount: TGLint;
  var MyFont: TUTrueTypeFontShared;
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

function TForm1.RequestDebugContext: Boolean;
begin
  Result := True;
end;

procedure TForm1.Initialize;
  var ShaderSource: String;
  var Ptr: Pointer;
  var i: Integer;
  var ErrorBuffer: array[0..511] of AnsiChar;
begin
  MyFont := TUTrueTypeFont.Create(LocalFile('fonts/FreeSerif.ttf'));
  glCreateVertexArrays(1, @VertexArray);
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
  UniformLines := glGetUniformLocation(Shader, PGLchar(PAnsiChar('lines')));
  UniformLineCount := glGetUniformLocation(Shader, PGLchar(PAnsiChar('line_count')));
  UniformCurves := glGetUniformLocation(Shader, PGLchar(PAnsiChar('curves')));
  UniformCurveCount := glGetUniformLocation(Shader, PGLchar(PAnsiChar('curve_count')));
  //UniformWVP := glGetUniformLocation(Shader, PGLchar(PAnsiChar('WVP')));
  //UniformTex0 := glGetUniformLocation(Shader, PGLchar(PAnsiChar('tex0')));
end;

procedure TForm1.Finalize;
begin
  glDeleteProgram(Shader);
  glDeleteVertexArrays(1, @VertexArray);
end;

procedure TForm1.Tick;
  const Bias = 0.0001;
  var Lines: TUVec2Array;
  procedure AddLine(const v0, v1: TUVec2);
    var i: Int32;
    var n: TUVec2;
  begin
    i := Length(Lines);
    SetLength(Lines, i + 2);
    n := (v1 - v0).Norm * Bias;
    Lines[i] := v0 + n;
    Lines[i + 1] := v1 - n;
  end;
  var Curves: TUVec2Array;
  procedure AddCurve(const v0, v1, v2: TUVec2);
    var i: Int32;
  begin
    i := Length(Curves);
    SetLength(Curves, i + 3);
    Curves[i] := UBezier(v0, v1, v2, Bias);
    Curves[i + 1] := v1;
    Curves[i + 2] := UBezier(v0, v1, v2, 1 - Bias);
  end;
  var W, V, P, WVP: TUMat;
  var i, j, n: Int32;
  var GlyphId: UInt32;
  var Glyph: TUTrueTypeFont.TGlyph;
  var p0, p1, p2: TUVec2;
begin
  {
  W := TUMat.Scaling(1, TextureSize.y / TextureSize.x, 1);
  W := W * TUMat.RotationY(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  V := TUMat.View(TUVec3.Make(0, 0.2, -1), TUVec3.Zero, TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.5, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;
  //}

  GlyphId := MyFont.Ptr.FindGlyph(UStrUTF8ToUTF32('Ж', i));
  Glyph := MyFont.Ptr.Glyphs[GlyphId];
  for i := 0 to High(Glyph.Contours) do
  begin
    for j := 0 to High(Glyph.Contours[i]) do
    begin
      if not Glyph.Contours[i][j].OnCurve then Continue;
      p0 := (Glyph.Contours[i][j].Pos - Glyph.Bounds.Min) / Glyph.Bounds.Size;
      n := (j + 1) mod Length(Glyph.Contours[i]);
      p1 := (Glyph.Contours[i][n].Pos - Glyph.Bounds.Min) / Glyph.Bounds.Size;
      if Glyph.Contours[i][n].OnCurve then
      begin
        AddLine(p0, p1);
        Continue;
      end;
      n := (j + 2) mod Length(Glyph.Contours[i]);
      p2 := (Glyph.Contours[i][n].Pos - Glyph.Bounds.Min) / Glyph.Bounds.Size;
      AddCurve(p0, p1, p2);
    end;
  end;
  //SetLength(Points, 2);
  //Points[0] := [0.1, 0.5];
  //Points[1] := [0.9, 0.5];
  //Lines[0] := [0, Length(Points)];

  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0.4, 1, 0.8, 1);
  glClear(GL_COLOR_BUFFER_BIT);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindVertexArray(VertexArray);
  glUseProgram(Shader);

  glUniform2fv(UniformLines, Length(Lines), @Lines[0]);
  glUniform1i(UniformLineCount, Length(Lines));
  glUniform2fv(UniformCurves, Length(Curves), @Curves[0]);
  glUniform1i(UniformCurveCount, Length(Curves));

  glDrawArrays(GL_TRIANGLES, 0, 6);
end;

end.

