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
  var UniformPoints: TGLint;
  var UniformContours: TGLint;
  var UniformContourCount: TGLint;
  var UniformXf: TGLint;
  var UniformXp: TGLint;
  var UniformSc: TGLint;
  var MyFont: TUTrueTypeFontShared;
protected
  function RequestDebugContext: Boolean; override;
public
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
  procedure DrawGlyph(const Glyph: TUTrueTypeFont.TGlyph; const Pos: TUVec2);
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
  UniformPoints := glGetUniformLocation(Shader, PGLchar(PAnsiChar('points')));
  UniformContours := glGetUniformLocation(Shader, PGLchar(PAnsiChar('contours')));
  UniformContourCount := glGetUniformLocation(Shader, PGLchar(PAnsiChar('contour_count')));
  UniformXf := glGetUniformLocation(Shader, PGLchar(PAnsiChar('xf')));
  UniformXp := glGetUniformLocation(Shader, PGLchar(PAnsiChar('xp')));
  UniformSc := glGetUniformLocation(Shader, PGLchar(PAnsiChar('sc')));
  //UniformWVP := glGetUniformLocation(Shader, PGLchar(PAnsiChar('WVP')));
  //UniformTex0 := glGetUniformLocation(Shader, PGLchar(PAnsiChar('tex0')));
end;

procedure TForm1.Finalize;
begin
  glDeleteProgram(Shader);
  glDeleteVertexArrays(1, @VertexArray);
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
  var GlyphId: UInt32;
  var Glyph: TUTrueTypeFont.TGlyph;
  var i, n: Int32;
  var Pos: TUVec2;
  const Str = 'Ййёj๛!มกำล๛Ж';
begin
  glViewport(0, 0, ClientWidth, ClientHeight);
  glClearColor(0.4, 1, 0.8, 1);
  glClear(GL_COLOR_BUFFER_BIT);

  //WriteLn(UStrUTF32ToUTF8(3675));
  GlyphId := MyFont.Ptr.FindGlyph(3675);
  Pos := TUVec2.Make(0, 0);
  i := 1;
  //while i <= Length(Str) do
  begin
    GlyphId := MyFont.Ptr.FindGlyph(UStrUTF8ToUTF32(Str, n, i));
    Glyph := MyFont.Ptr.Glyphs[GlyphId];
    DrawGlyph(Glyph, Pos);
    Inc(i, n);
    Pos.x := Pos.x + Glyph.Advance * 0.0025;
  end;
  {
  W := TUMat.Scaling(1, TextureSize.y / TextureSize.x, 1);
  W := W * TUMat.RotationY(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  V := TUMat.View(TUVec3.Make(0, 0.2, -1), TUVec3.Zero, TUVec3.Make(0, 1, 0));
  P := TUMat.Proj(UPi * 0.5, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;
  //}
end;

procedure TForm1.DrawGlyph(const Glyph: TUTrueTypeFont.TGlyph; const Pos: TUVec2);
  var Points: TUVec2Array;
  procedure AddPoint(const v: TUVec2);
  begin
    specialize UArrAppend<TUVec2>(Points, v);
  end;
  var Contours: TUVec2iArray;
  var i, j, n, c: Int32;
  var p0, p1, p2: TUVec2;
  var GlyphSize: TUFloat;
  var Xf: TUMat2;
  var Xp: TUVec4;
  var Sc: TUVec4;
  var MaxRatio: TUFloat;
  var Aspect: TUVec2;
  var Scale: TUFloat;
  const Area = 1 / 1;
begin
  GlyphSize := MyFont.Ptr.LineHeight;
  Scale := 1.9 * Area;
  Contours := nil;
  for i := 0 to High(Glyph.Contours) do
  begin
    if Length(Glyph.Contours[i]) = 0 then Continue;
    SetLength(Contours, Length(Contours) + 1);
    Contours[High(Contours)][0] := Length(Points);
    c := 0;
    for j := 0 to High(Glyph.Contours[i]) do
    begin
      n := j mod Length(Glyph.Contours[i]);
      if not Glyph.Contours[i][n].OnCurve then Continue;
      Inc(c);
      p0 := (Glyph.Contours[i][n].Pos - Glyph.Bounds.Min) / GlyphSize;
      AddPoint(p0);
      n := (j + 1) mod Length(Glyph.Contours[i]);
      p1 := (Glyph.Contours[i][n].Pos - Glyph.Bounds.Min) / GlyphSize;
      if Glyph.Contours[i][n].OnCurve then
      begin
        AddPoint((p0 + p1) * 0.5);
      end
      else
      begin
        AddPoint(p1);
      end;
    end;
    p0 := (Glyph.Contours[i][0].Pos - Glyph.Bounds.Min) / GlyphSize;
    AddPoint(p0);
    Contours[High(Contours)][1] := c;
  end;
  MaxRatio := 1 / UMax(ClientWidth, ClientHeight);
  Aspect := TUVec2.Make(ClientWidth * MaxRatio, ClientHeight * MaxRatio);
  Xp := TUVec4.Make(
    Pos.x, Pos.y,
    Glyph.Bounds.Size.x / GlyphSize,
    Glyph.Bounds.Size.y / GlyphSize
  );
  Xf := TUMat2.Make(
    Xp.z * Scale * Aspect.y, 0, 0, Xp.w * Scale * Aspect.x
  );
  Sc := TUVec4.Make(
    1 / 10, 1 / 10, 0, 0
  );
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindVertexArray(VertexArray);
  glUseProgram(Shader);

  glUniform2fv(UniformPoints, Length(Points), @Points[0]);
  glUniform2iv(UniformContours, Length(Contours), @Contours[0]);
  glUniform1i(UniformContourCount, Length(Contours));
  glUniformMatrix2fv(UniformXf, 1, GL_FALSE, @Xf);
  glUniform4fv(UniformXp, 1, @Xp);
  glUniform4fv(UniformSc, 1, @Sc);

  glDrawArrays(GL_TRIANGLES, 0, 6);
end;

end.

