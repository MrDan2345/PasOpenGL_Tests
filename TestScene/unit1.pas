unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  CommonUtils, MediaUtils, Setup, Math;

type TGLuintArray = array of TGLuint;
type TNode = class;
type TMaterial = class;

type TShader = class(TURefClass)
public
  type TSkinInfo = record
    BoneCount: Int32;
    BoneWeights: Int32;
  end;
  type PSkinInfo = ^TSkinInfo;
private
  var _Handle: TGLuint;
  var _UniformWVP: TGLint;
  var _UniformMatColor: TGLint;
  var _UniformTex0: TGLint;
  var _UniformBone: TGLint;
  class var _ShaderMap: specialize TUMap<UInt64, TShader>;
public
  property Handle: TGLuint read _Handle;
  property UniformWVP: TGLint read _UniformWVP;
  property UniformMatColor: TGLint read _UniformMatColor;
  property UniformTex0: TGLint read _UniformTex0;
  property UniformBone: TGLint read _UniformBone;
  class function AutoShader(
    const VertexDescriptor: TUVertexDescriptor;
    const Material: TMaterial;
    const SkinInfo: PSkinInfo = nil
  ): TShader;
  constructor Create(const vs, ps: String);
  destructor Destroy; override;
  procedure Use;
  function UniformLocation(const UniformName: String): TGLint;
end;
type TShaderShared = specialize TUSharedRef<TShader>;
type TShaderList = array of TShaderShared;

type TMesh = class (TURefClass)
public
  type TSubset = class
  public
    var VertexDescriptor: TUVertexDescriptor;
    var vb: TGLuint;
    var VertexBuffer: TGLuint;
    var VertexSize: TGLuint;
    var VertexCount: TGluint;
    var IndexBuffer: TGLuint;
    var IndexSize: TGLuint;
    var IndexCount: TGLuint;
    var IndexFormat: TGLenum;
    constructor Create(const MeshSubset: TUSceneData.TMeshInterface.TSubset);
    destructor Destroy; override;
  end;
  type TSubsetList = array of TSubset;
private
  var _Subsets: TSubsetList;
public
  property Subsets: TSubsetList read _Subsets;
  constructor Create(const MeshData: TUSceneData.TMeshInterface);
  destructor Destroy; override;
  procedure DrawSubset(const Index: Int32);
end;
type TMeshShared = specialize TUSharedRef<TMesh>;

type TSkin = class (TURefClass)
public
  type TSubset = class
  public
    var VertexBuffer: TGLuint;
    var VertexCount: TGluint;
    var VertexSize: UInt32;
    var WeightCount: Int32;
    constructor Create(
      const MeshSubset: TUSceneData.TMeshInterface.TSubset;
      const SkinSubset: TUSceneData.TSkinInterface.TSubset
    );
    destructor Destroy; override;
  end;
  type TSubsetList = array of TSubset;
  type TJoint = record
    Node: String;
    Bind: TUMat;
  end;
  type TJointList = array of TJoint;
private
  var _Mesh: TMesh;
  var _Subsets: TSubsetList;
  var _Joints: TJointList;
  var _Bind: TUMat;
public
  property Mesh: TMesh read _Mesh;
  property Subsets: TSubsetList read _Subsets;
  property Joints: TJointList read _Joints;
  property Bind: TUMat read _Bind;
  constructor Create(const SkinData: TUSceneData.TSkinInterface);
  destructor Destroy; override;
end;
type TSkinShared = specialize TUSharedRef<TSkin>;

type TTexture = class (TURefClass)
private
  var _Handle: TGLuint;
public
  property Handle: TGLuint read _Handle;
  constructor Create(const ImageData: TUSceneData.TImageInterface);
  destructor Destroy; override;
end;
type TTextureShared = specialize TUSharedRef<TTexture>;

type TMaterial = class (TURefClass)
public
  type TMaterialList = array of TMaterial;
private
  var _Texture: TTextureShared;
  var _Color: TUColor;
public
  property Texture: TTextureShared read _Texture;
  property Color: TUColor read _Color;
  constructor Create(const MaterialData: TUSceneData.TMaterialInterface);
  destructor Destroy; override;
end;
type TMaterialShared = specialize TUSharedRef<TMaterial>;

type TAnimation = class (TURefClass)
public
  type TTrack = class
  public
    type TKey = record
      var Time: TUFloat;
      var Value: TUMat;
      var Interpolation: TUSceneData.TAnimationInterface.TKeyInterpolation;
    end;
    type TKeyList = array of TKey;
  private
    var _Name: String;
    var _Keys: TKeyList;
    var _Target: TNode;
    var _MaxTime: TUFloat;
    function FindKey(const Time: TUFloat): Int32;
  public
    property Name: String read _Name;
    property Keys: TKeyList read _Keys;
    property Target: TNode read _Target;
    property MaxTime: TUFloat read _MaxTime;
    function Sample(const Time: TUFloat; const Loop: Boolean = True): TUMat;
    constructor Create(const TrackData: TUSceneData.TAnimationInterface.TTrack);
    destructor Destroy; override;
  end;
  type TTrackList = array of TTrack;
private
  var _Tracks: TTrackList;
public
  property Tracks: TTrackList read _Tracks;
  constructor Create(const AnimationData: TUSceneData.TAnimationInterface);
  destructor Destroy; override;
end;
type TAnimationShared = specialize TUSharedRef<TAnimation>;

type TNode = class (TURefClass)
public
  type TNodeList = array of TNode;
  type TAttachment = class
  private
    var _Node: TNode;
    procedure SetNode(const Value: TNode);
  public
    property Node: TNode read _Node write SetNode;
    procedure Setup; virtual;
  end;
  type TAttachmentMesh = class (TAttachment)
  private
    var _Mesh: TMesh;
    var _Materials: TMaterial.TMaterialList;
    var _Shaders: TShaderList;
    var _VertexArrays: TGLuintArray;
  public
    property Mesh: TMesh read _Mesh;
    property Materials: TMaterial.TMaterialList read _Materials;
    property Shaders: TShaderList read _Shaders;
    property VertexArrays: TGLuintArray read _VertexArrays;
    constructor Create(const AttachData: TUSceneData.TAttachmentMesh);
    destructor Destroy; override;
    procedure Setup; override;
  end;
  type TAttachmentSkin = class (TAttachment)
  public
    type TJointBinding = record
      var Bind: TUMat;
      var Node: TNode;
    end;
    type TJointBindingList = array of TJointBinding;
  private
    var _Skin: TSkin;
    var _Materials: TMaterial.TMaterialList;
    var _Shaders: TShaderList;
    var _VertexArrays: TGLuintArray;
    var _Pose: TUMatArray;
    var _JointBindings: TNodeList;
  public
    property Skin: TSkin read _Skin;
    property Materials: TMaterial.TMaterialList read _Materials;
    property Shaders: TShaderList read _Shaders;
    property VertexArrays: TGLuintArray read _VertexArrays;
    property Pose: TUMatArray read _Pose;
    constructor Create(const AttachData: TUSceneData.TAttachmentSkin);
    destructor Destroy; override;
    procedure Setup; override;
    procedure UpdatePose;
  end;
  type TAttachmentList = array of TAttachment;
private
  var _Name: String;
  var _Parent: TNode;
  var _Children: TNodeList;
  var _Attachments: TAttachmentList;
  var _Transform: TUMat;
  procedure ChildAdd(const Child: TNode); inline;
  procedure ChildRemove(const Child: TNode); inline;
  procedure AttachAdd(const Attach: TAttachment);
  procedure AttachRemove(const Attach: TAttachment);
  procedure SetParent(const Value: TNode);
  procedure ApplyTransform(const Value: TUMat);
  procedure SetTransform(const Value: TUMat);
  function GetLocalTransform: TUMat; inline;
  procedure SetLocalTransform(const Value: TUMat);
public
  property Name: String read _Name;
  property Parent: TNode read _Parent write SetParent;
  property Children: TNodeList read _Children;
  property Attachments: TAttachmentList read _Attachments;
  property Transform: TUMat read _Transform write SetTransform;
  property LocalTransform: TUMat read GetLocalTransform write SetLocalTransform;
  constructor Create(
    const AParent: TNode;
    const NodeData: TUSceneData.TNodeInterface
  );
  destructor Destroy; override;
  procedure SetupAttachments(const NodeData: TUSceneData.TNodeInterface);
  procedure Setup;
end;
type TNodeShared = specialize TUSharedRef<TNode>;

type TForm1 = class(TCommonForm)
private
  var Meshes: array of TMeshShared;
  var Skins: array of TSkinShared;
  var Textures: array of TTextureShared;
  var Materials: array of TMaterialShared;
  var Animations: array of TAnimationShared;
  var RootNode: TNodeShared;
  var TextureRemap: specialize TUMap<Pointer, TTextureShared>;
  var MeshRemap: specialize TUMap<Pointer, TMeshShared>;
  var SkinRemap: specialize TUMap<Pointer, TSkinShared>;
  var MaterialRemap: specialize TUMap<Pointer, TMaterialShared>;
  var NodeRemap: specialize TUMap<Pointer, TNode>;
  var AppStartTime: UInt64;
  var LoadDir: String;
  var TaskLoad: specialize TUTask<TNodeShared>;
  procedure ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
  function TF_Load(const Args: array of const): TNodeShared;
protected
  function RequestDebugContext: Boolean; override;
public
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
  procedure PrintScene;
end;

var Form1: TForm1;

implementation

{$R *.lfm}

class function TShader.AutoShader(
  const VertexDescriptor: TUVertexDescriptor;
  const Material: TMaterial;
  const SkinInfo: PSkinInfo
): TShader;
  function MakeHash: UInt64;
    var n: UInt32;
  begin
    Result := 0;
    n := Length(VertexDescriptor);
    Result := UCRC64(Result, @n, SizeOf(n));
    Result := UCRC64(Result, @VertexDescriptor[0], n * SizeOf(VertexDescriptor));
    if Assigned(SkinInfo) then
    begin
      Result := UCRC64(Result, SkinInfo, SizeOf(SkinInfo^));
    end;
  end;
  function AttributeName(const Attribute: TUVertexAttribute): String;
  begin
    case Attribute.Semantic of
      as_position: Result := 'position';
      as_normal: Result := 'normal';
      as_tangent: Result := 'tangent';
      as_binormal: Result := 'binormal';
      as_color: Result := 'color';
      as_texcoord: Result := 'texcoord' + IntToStr(Attribute.SetNumber);
      else Result := '';
    end;
  end;
  var Hash: UInt64;
  var Attrib: TUVertexAttribute;
  var vs, ps, Inputs, Outputs, AttName, AttSize: String;
  var i, BoneCount, AttribIndex: Int32;
begin
  Hash := MakeHash;
  Result := _ShaderMap.FindValueByKey(Hash);
  if Assigned(Result) then Exit;
  vs := '#version 450 core'#$D#$A;
  Inputs := '';
  Outputs := '';
  AttribIndex := 0;
  for Attrib in VertexDescriptor do
  begin
    AttName := AttributeName(Attrib);
    AttSize := IntToStr(Attrib.DataCount);
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in vec' + AttSize + ' in_' + AttName + ';'#$D#$A;
    if Attrib.Semantic <> as_position then
    begin
      Outputs += 'layout (location = ' + IntToStr(AttribIndex) + ') out vec' + AttSize + ' out_' + AttName + ';'#$D#$A;
    end;
    Inc(AttribIndex);
  end;
  if Assigned(SkinInfo) then
  begin
    AttSize := IntToStr(SkinInfo^.BoneWeights);
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in uvec' + AttSize + ' in_bone_index;'#$D#$A;
    Inc(AttribIndex);
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in vec' + AttSize + ' in_bone_weight;'#$D#$A;
    Inc(AttribIndex);
  end;
  vs += Inputs + Outputs;
  vs += 'uniform mat4x4 WVP;'#$D#$A;
  if Assigned(SkinInfo) then
  begin
    BoneCount := (((SkinInfo^.BoneCount - 1) div 100) + 1) * 100;
    vs += 'uniform mat4x4 Bone[' + IntToStr(BoneCount) + '];'#$D#$A;
  end;
  vs += 'void main() {'#$D#$A;
  if Assigned(SkinInfo) then
  begin
    vs += '  mat4x4 S = ('#$D#$A;
    for i := 0 to SkinInfo^.BoneWeights - 1 do
    begin
      vs += '    (Bone[in_bone_index[' + IntToStr(i) + ']] * in_bone_weight[' + IntToStr(i) + '])';
      if i < SkinInfo^.BoneWeights - 1 then vs += ' + ';
      vs += #$D#$A;
    end;
    vs += '  );'#$D#$A;
  end;
  for i := 0 to High(VertexDescriptor) do
  begin
    if VertexDescriptor[i].Semantic = as_position then
    begin
      if Assigned(SkinInfo) then
      begin
        vs += '  vec4 position = vec4((vec4(in_position, 1.0) * S).xyz, 1.0);'#$D#$A;
      end
      else
      begin
        vs += '  vec4 position = vec4(in_position, 1.0);'#$D#$A;
      end;
      vs += '  gl_Position = position * WVP;'#$D#$A;
    end
    else if VertexDescriptor[i].Semantic = as_texcoord then
    begin
      AttName := AttributeName(VertexDescriptor[i]);
      vs += '  out_' + AttName + ' = vec2(in_' + AttName + '.x, ' + '1-in_' + AttName + '.y);'#$D#$A;
    end
    else
    begin
      AttName := AttributeName(VertexDescriptor[i]);
      vs += '  out_' + AttName + ' = in_' + AttName + ';'#$D#$A;
    end;
  end;
  vs += '}'#$D#$A;
  ps := '#version 450 core'#$D#$A;
  for i := 0 to High(VertexDescriptor) do
  begin
    if VertexDescriptor[i].Semantic = as_position then Continue;
    AttName := AttributeName(VertexDescriptor[i]);
    AttSize := IntToStr(VertexDescriptor[i].DataCount);
    ps += 'layout (location = ' + IntToStr(i) + ') in vec' + AttSize + ' in_' + AttName + ';'#$D#$A;
  end;
  ps += 'out vec4 out_color;'#$D#$A;
  if Material.Texture.IsValid then
  begin
    ps += 'uniform sampler2D tex0;'#$D#$A;
  end;
  ps += 'uniform vec4 m_color;'#$D#$A;
  ps += 'void main() {'#$D#$A;
  ps += '  out_color = m_color;'#$D#$A;
  if Material.Texture.IsValid then
  begin
    ps += '  out_color *= texture(tex0, in_texcoord0.xy);'#$D#$A;
  end;
  ps += '}'#$D#$A;
  //UStrToFile('vs_' + IntToStr(Hash) + '.txt', vs);
  //UStrToFile('ps_' + IntToStr(Hash) + '.txt', ps);
  Result := TShader.Create(vs, ps);
  _ShaderMap.Add(Hash, Result);
end;

constructor TShader.Create(const vs, ps: String);
  var VertexShader, PixelShader: TGLuint;
  var Ptr: Pointer;
  var i: Int32;
  var ErrorBuffer: array[0..511] of AnsiChar;
begin
  VertexShader := glCreateShader(GL_VERTEX_SHADER);
  Ptr := PAnsiChar(vs);
  glShaderSource(VertexShader, 1, @Ptr, nil);
  glCompileShader(VertexShader);
  glGetShaderiv(VertexShader, GL_COMPILE_STATUS, @i);
  if i = GL_FALSE then
  begin
    glGetShaderInfoLog(VertexShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  PixelShader := glCreateShader(GL_FRAGMENT_SHADER);
  Ptr := PAnsiChar(ps);
  glShaderSource(PixelShader, 1, @Ptr, nil);
  glCompileShader(PixelShader);
  glGetShaderiv(PixelShader, GL_COMPILE_STATUS, @i);
  if i = GL_FALSE then
  begin
    glGetShaderInfoLog(PixelShader, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  _Handle := glCreateProgram();
  glAttachShader(_Handle, VertexShader);
  glAttachShader(_Handle, PixelShader);
  glLinkProgram(_Handle);
  glGetProgramiv(_Handle, GL_LINK_STATUS, @i);
  if i = GL_FALSE then
  begin
    glGetProgramInfoLog(_Handle, Length(ErrorBuffer), @i, @ErrorBuffer);
    WriteLn(ErrorBuffer);
  end;
  glDeleteShader(PixelShader);
  glDeleteShader(VertexShader);
  _UniformWVP := UniformLocation('WVP');
  _UniformMatColor := UniformLocation('m_color');
  _UniformTex0 := UniformLocation('tex0');
  _UniformBone := UniformLocation('Bone');
end;

destructor TShader.Destroy;
begin
  _ShaderMap.RemoveByValue(Self);
  glDeleteProgram(_Handle);
  inherited Destroy;
end;

procedure TShader.Use;
begin
  glUseProgram(_Handle);
end;

function TShader.UniformLocation(const UniformName: String): TGLint;
begin
  Result := glGetUniformLocation(_Handle, PGLchar(PAnsiChar(UniformName)));
end;

constructor TMesh.TSubset.Create(const MeshSubset: TUSceneData.TMeshInterface.TSubset);
begin
  VertexDescriptor := MeshSubset.VertexDescriptor;
  VertexCount := MeshSubset.VertexCount;
  IndexCount := MeshSubset.IndexCount;
  VertexSize := MeshSubset.VertexSize;
  IndexSize := MeshSubset.IndexSize;
  if IndexSize = 4 then
  begin
    IndexFormat := GL_UNSIGNED_INT;
  end
  else
  begin
    IndexFormat := GL_UNSIGNED_SHORT;
  end;
  //{ DSA
  glCreateBuffers(1, @VertexBuffer);
  glNamedBufferStorage(
    VertexBuffer, VertexCount * VertexSize,
    MeshSubset.VertexData, 0
  );
  glCreateBuffers(1, @IndexBuffer);
  glNamedBufferStorage(
    IndexBuffer, IndexCount * IndexSize,
    MeshSubset.IndexData, 0
  );
  //}
  { pre DSA
  glGenBuffers(1, @VertexBuffer);
  glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer);
  glBufferData(GL_ARRAY_BUFFER, VertexCount * VertexSize, MeshSubset.VertexData, GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glGenBuffers(1, @IndexBuffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, IndexBuffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, IndexCount * IndexSize, MeshSubset.IndexData, GL_STATIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  //}
end;

destructor TMesh.TSubset.Destroy;
begin
  glDeleteBuffers(1, @IndexBuffer);
  glDeleteBuffers(1, @VertexBuffer);
  inherited Destroy;
end;

constructor TMesh.Create(const MeshData: TUSceneData.TMeshInterface);
  var i: Int32;
begin
  SetLength(_Subsets, Length(MeshData.Subsets));
  for i := 0 to High(_Subsets) do
  begin
    _Subsets[i] := TSubset.Create(MeshData.Subsets[i]);
  end;
end;

destructor TMesh.Destroy;
begin
  specialize UArrClear<TSubset>(_Subsets);
  inherited Destroy;
end;

procedure TMesh.DrawSubset(const Index: Int32);
  var Subset: TSubset;
begin
  Subset := _Subsets[Index];
  glDrawElements(
    GL_TRIANGLES,
    Subset.IndexCount,
    Subset.IndexFormat,
    Pointer(0)
  );
end;

constructor TSkin.TSubset.Create(
  const MeshSubset: TUSceneData.TMeshInterface.TSubset;
  const SkinSubset: TUSceneData.TSkinInterface.TSubset);
begin
  VertexCount := MeshSubset.VertexCount;
  VertexSize := SkinSubset.VertexSize;
  WeightCount := SkinSubset.WeightCount;
  //{ DSA
  glCreateBuffers(1, @VertexBuffer);
  glNamedBufferStorage(
    VertexBuffer, VertexCount * VertexSize,
    SkinSubset.VertexData, 0
  );
  //}
  { pre DSA
  glGenBuffers(1, @VertexBuffer);
  glBindBuffer(GL_ARRAY_BUFFER, VertexBuffer);
  glBufferData(GL_ARRAY_BUFFER, VertexCount * VertexSize, SkinSubset.VertexData, GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  //}
end;

destructor TSkin.TSubset.Destroy;
begin
  glDeleteBuffers(1, @VertexBuffer);
  inherited Destroy;
end;

constructor TSkin.Create(const SkinData: TUSceneData.TSkinInterface);
  var SubsetId, JointId: Int32;
begin
  _Mesh := Form1.MeshRemap.FindValueByKey(SkinData.Mesh).Ptr;
  SetLength(_Subsets, Length(_Mesh.Subsets));
  for SubsetId := 0 to High(_Subsets) do
  begin
    _Subsets[SubsetId] := TSubset.Create(
      SkinData.Mesh.Subsets[SubsetId],
      SkinData.Subsets[SubsetId]
    );
  end;
  _Bind := SkinData.ShapeBind;
  SetLength(_Joints, Length(SkinData.Joints));
  for JointId := 0 to High(_Joints) do
  begin
    _Joints[JointId].Node := SkinData.Joints[JointId].Name;
    _Joints[JointId].Bind := SkinData.Joints[JointId].Bind;
  end;
end;

destructor TSkin.Destroy;
begin
  specialize UArrClear<TSubset>(_Subsets);
  inherited Destroy;
end;

constructor TTexture.Create(const ImageData: TUSceneData.TImageInterface);
  var Image: TUImageDataShared;
  var TextureFormat, TextureType: TGLenum;
  var MipLevels: UInt32;
begin
  //Image := ULoadImageData('../Assets/siren/' + ImageData.FileName);
  //Image := ULoadImageData('../Assets/' + ImageData.FileName);
  Image := ULoadImageData(Form1.LoadDir + '/' + ImageData.FileName);
  if not Image.IsValid then
  begin
    _Handle := 0;
    Exit;
  end;
  MipLevels := Round(Math.Log2(UMax(Image.Ptr.Width, Image.Ptr.Height)));
  Form1.ImageFormatToGL(Image.Ptr.Format, TextureFormat, TextureType);
  //{ DSA
  glCreateTextures(GL_TEXTURE_2D, 1, @_Handle);
  glTextureParameteri(_Handle, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTextureParameteri(_Handle, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTextureParameteri(_Handle, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTextureParameteri(_Handle, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTextureStorage2D(
    _Handle, MipLevels, GL_RGBA8,
    Image.Ptr.Width, Image.Ptr.Height
  );
  glTextureSubImage2D(
    _Handle, 0, 0, 0, Image.Ptr.Width, Image.Ptr.Height,
    TextureFormat, TextureType, Image.Ptr.Data
  );
  glGenerateTextureMipmap(_Handle);
  //}
  { pre DSA
  glGenTextures(1, @_Handle);
  glBindTexture(GL_TEXTURE_2D, _Handle);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexImage2D(
    GL_TEXTURE_2D, 0, GL_RGB,
    Image.Ptr.Width, Image.Ptr.Height, 0,
    TextureFormat, TextureType, Image.Ptr.Data
  );
  glGenerateMipmap(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);
  //}
end;

destructor TTexture.Destroy;
begin
  if _Handle > 0 then glDeleteTextures(1, @_Handle);
  inherited Destroy;
end;

constructor TMaterial.Create(const MaterialData: TUSceneData.TMaterialInterface);
  var i, j: Int32;
  var Param: TUSceneData.TMaterialInterface.TParam;
  var ParamVec4: TUSceneData.TMaterialInterface.TParamVec4 absolute Param;
  var Image: TUSceneData.TMaterialInterface.TParamImage absolute Param;
begin
  _Color := TUColor.White;
  for i := 0 to High(MaterialData.Params) do
  begin
    Param := MaterialData.Params[i];
    if LowerCase(Param.Name) <> 'diffuse' then Continue;
    if Param is TUSceneData.TMaterialInterface.TParamVec4 then
    begin
      _Color := ParamVec4.Value;
    end
    else if Param is TUSceneData.TMaterialInterface.TParamImage then
    begin
      j := Form1.TextureRemap.FindIndexByKey(Image.Image);
      if j > -1 then _Texture := Form1.TextureRemap[j];
    end;
  end;
end;

destructor TMaterial.Destroy;
begin
  inherited Destroy;
end;

constructor TAnimation.Create(const AnimationData: TUSceneData.TAnimationInterface);
  var i: Int32;
begin
  SetLength(_Tracks, Length(AnimationData.Tracks));
  for i := 0 to High(_Tracks) do
  begin
    _Tracks[i] := TTrack.Create(AnimationData.Tracks[i]);
  end;
end;

destructor TAnimation.Destroy;
begin
  inherited Destroy;
end;

function TAnimation.TTrack.FindKey(const Time: TUFloat): Int32;
  var i, l, h, m: Int32;
begin
  l := 0; h := High(_Keys);
  i := h - l;
  while i > 1 do
  begin
    m := l + (i shr 1);
    if _Keys[m].Time > Time then h := m else l := m;
    i := h - l;
  end;
  Result := l;
end;

function TAnimation.TTrack.Sample(const Time: TUFloat; const Loop: Boolean): TUMat;
  var k0, k1: UInt32;
  var t: TUFloat;
begin
  if (Length(_Keys) < 1) then Exit(TUMat.Identity);
  if not Loop then
  begin
    if Time <= _Keys[0].Time then
    begin
      Exit(_Keys[0].Value);
    end;
    if Time >= _Keys[High(_Keys)].Time then
    begin
      Exit(_Keys[High(_Keys)].Value);
    end;
  end;
  t := Time mod _Keys[High(_Keys)].Time;
  k0 := FindKey(t);
  case _Keys[k0].Interpolation of
    ki_step: Exit(_Keys[k0].Value);
    ki_linear:
    begin
      k1 := (k0 + 1) mod Length(_Keys);
      if k1 < k0 then USwap(k0, k1);
      t := (t - _Keys[k0].Time) / (_Keys[k1].Time - _Keys[k0].Time);
      Exit(ULerp(_Keys[k0].Value, _Keys[k1].Value, t));
    end;
  end;
end;

constructor TAnimation.TTrack.Create(const TrackData: TUSceneData.TAnimationInterface.TTrack);
  var i: Int32;
begin
  _Target := Form1.NodeRemap.FindValueByKey(TrackData.Target);
  SetLength(_Keys, Length(TrackData.Keys));
  for i := 0 to High(_Keys) do
  begin
    _Keys[i].Interpolation := TrackData.Keys[i].Interpolation;
    _Keys[i].Time := TrackData.Keys[i].Time;
    _Keys[i].Value := TrackData.Keys[i].Value;
  end;
end;

destructor TAnimation.TTrack.Destroy;
begin
  inherited Destroy;
end;

procedure TNode.TAttachment.SetNode(const Value: TNode);
begin
  if _Node = Value then Exit;
  if Assigned(_Node) then _Node.AttachRemove(Self);
  _Node := Value;
  if Assigned(_Node) then _Node.AttachAdd(Self);
end;

procedure TNode.TAttachment.Setup;
begin
end;

constructor TNode.TAttachmentMesh.Create(
  const AttachData: TUSceneData.TAttachmentMesh
);
  var i: Int32;
begin
  inherited Create;
  _Mesh := Form1.MeshRemap.FindValueByKey(AttachData.Mesh).Ptr;
  SetLength(_Materials, Length(AttachData.MaterialBindings));
  for i := 0 to High(_Materials) do
  begin
    if Assigned(AttachData.MaterialBindings[i].BaseMaterial) then
    begin
      _Materials[i] := Form1.MaterialRemap.FindValueByKey(
        AttachData.MaterialBindings[i].BaseMaterial
      ).Ptr;
    end
    else
    begin
      _Materials[i] := nil;
    end;
  end;
end;

destructor TNode.TAttachmentMesh.Destroy;
begin
  glDeleteVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  inherited Destroy;
end;

procedure TNode.TAttachmentMesh.Setup;
  var SubsetId, i: Int32;
  var Subset: TMesh.TSubset;
  var AttribOffset: GLuint;
  //var AttribOffset: Pointer;
  var vd: TUVertexDescriptor;
  var VertexArray: TGLuint;
begin
  if Length(_VertexArrays) > 0 then Exit;
  SetLength(_VertexArrays, Length(_Mesh.Subsets));
  glCreateVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  //glGenVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  for SubsetId := 0 to High(_Mesh.Subsets) do
  begin
    VertexArray := _VertexArrays[SubsetId];
    Subset := _Mesh.Subsets[SubsetId];
    vd := Subset.VertexDescriptor;
    specialize UArrAppend<TShaderShared>(
      _Shaders, TShader.AutoShader(vd, _Materials[SubsetId])
    );
    glVertexArrayVertexBuffer(
      VertexArray,
      0, Subset.VertexBuffer,
      0, Subset.VertexSize
    );
    //glBindVertexArray(_VertexArrays[SubsetId]);
    //glBindBuffer(GL_ARRAY_BUFFER, Subset.VertexBuffer);
    //AttribOffset := nil;
    AttribOffset := 0;
    for i := 0 to High(vd) do
    begin
      glEnableVertexArrayAttrib(VertexArray, i);
      glVertexArrayAttribFormat(
        VertexArray, i, vd[i].DataCount,
        GL_FLOAT, GL_FALSE, AttribOffset
      );
      glVertexArrayAttribBinding(VertexArray, i, 0);
      //glVertexAttribPointer(
      //  i, vd[i].DataCount, GL_FLOAT, GL_FALSE,
      //  Subset.VertexSize, AttribOffset
      //);
      //glEnableVertexAttribArray(i);
      AttribOffset += vd[i].Size;
    end;
    glVertexArrayElementBuffer(
      VertexArray,
      Subset.IndexBuffer
    );
    //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Subset.IndexBuffer);
  end;
  glBindVertexArray(0);
end;

constructor TNode.TAttachmentSkin.Create(const AttachData: TUSceneData.TAttachmentSkin);
  var i: Int32;
begin
  inherited Create;
  _Skin := Form1.SkinRemap.FindValueByKey(AttachData.Skin).Ptr;
  SetLength(_Materials, Length(AttachData.MaterialBindings));
  for i := 0 to High(_Materials) do
  begin
    if Assigned(AttachData.MaterialBindings[i].BaseMaterial) then
    begin
      _Materials[i] := Form1.MaterialRemap.FindValueByKey(
        AttachData.MaterialBindings[i].BaseMaterial
      ).Ptr;
    end
    else
    begin
      _Materials[i] := nil;
    end;
  end;
  SetLength(_Pose, Length(_Skin.Joints));
  SetLength(_JointBindings, Length(_Skin.Joints));
  for i := 0 to High(_Pose) do
  begin
    _JointBindings[i] := Form1.NodeRemap.FindValueByKey(AttachData.JointBindings[i]);
    WriteLn(_JointBindings[i].Name);
  end;
  UpdatePose;
end;

destructor TNode.TAttachmentSkin.Destroy;
begin
  glDeleteVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  inherited Destroy;
end;

procedure TNode.TAttachmentSkin.Setup;
  var SubsetId, i: Int32;
  var MeshSubset: TMesh.TSubset;
  var SkinSubset: TSkin.TSubset;
  var SkinInfo: TShader.TSkinInfo;
  //var AttribOffset: Pointer;
  var AttribOffset: GLuint;
  var vd: TUVertexDescriptor;
  var VertexArray: TGLuint;
begin
  if Length(_VertexArrays) > 0 then Exit;
  SetLength(_VertexArrays, Length(_Skin.Subsets));
  glCreateVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  //glGenVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  for SubsetId := 0 to High(_Skin.Subsets) do
  begin
    SkinSubset := _Skin.Subsets[SubsetId];
    MeshSubset := _Skin.Mesh.Subsets[SubsetId];
    VertexArray := _VertexArrays[SubsetId];
    vd := MeshSubset.VertexDescriptor;
    SkinInfo.BoneCount := Length(_Skin.Joints);
    SkinInfo.BoneWeights := SkinSubset.WeightCount;
    specialize UArrAppend<TShaderShared>(
      _Shaders, TShader.AutoShader(vd, _Materials[SubsetId], @SkinInfo)
    );
    glVertexArrayVertexBuffer(
      VertexArray,
      0, MeshSubset.VertexBuffer,
      0, MeshSubset.VertexSize
    );
    //glBindVertexArray(_VertexArrays[SubsetId]);
    //glBindBuffer(GL_ARRAY_BUFFER, MeshSubset.VertexBuffer);
    AttribOffset := 0;
    //AttribOffset := nil;
    for i := 0 to High(vd) do
    begin
      glEnableVertexArrayAttrib(VertexArray, i);
      glVertexArrayAttribFormat(
        VertexArray, i, vd[i].DataCount,
        GL_FLOAT, GL_FALSE, AttribOffset
      );
      glVertexArrayAttribBinding(VertexArray, i, 0);
      //glVertexAttribPointer(
      //  i, vd[i].DataCount, GL_FLOAT, GL_FALSE,
      //  MeshSubset.VertexSize, AttribOffset
      //);
      //glEnableVertexAttribArray(i);
      AttribOffset += vd[i].Size;
    end;
    glVertexArrayVertexBuffer(
      VertexArray,
      1, SkinSubset.VertexBuffer,
      0, SkinSubset.VertexSize
    );
    //glBindBuffer(GL_ARRAY_BUFFER, SkinSubset.VertexBuffer);
    //AttribOffset := nil;
    AttribOffset := 0;
    i := Length(vd);
    glEnableVertexArrayAttrib(VertexArray, i);
    glVertexArrayAttribIFormat(
      VertexArray, i, SkinInfo.BoneWeights,
      GL_UNSIGNED_INT, AttribOffset
    );
    glVertexArrayAttribBinding(VertexArray, i, 1);
    Inc(i);
    AttribOffset += SkinInfo.BoneWeights * SizeOf(UInt32);
    glEnableVertexArrayAttrib(VertexArray, i);
    glVertexArrayAttribFormat(
      VertexArray, i, SkinInfo.BoneWeights,
      GL_FLOAT, GL_FALSE, AttribOffset
    );
    glVertexArrayAttribBinding(VertexArray, i, 1);
    //glVertexAttribIPointer(
    //  i, SkinInfo.BoneWeights, GL_UNSIGNED_INT,
    //  SkinSubset.VertexSize, AttribOffset
    //);
    //glEnableVertexAttribArray(i); Inc(i);
    //AttribOffset += SkinInfo.BoneWeights * SizeOf(UInt32);
    //glVertexAttribPointer(
    //  i, SkinInfo.BoneWeights, GL_FLOAT, GL_FALSE,
    //  SkinSubset.VertexSize, AttribOffset
    //);
    //glEnableVertexAttribArray(i);
    glVertexArrayElementBuffer(VertexArray, MeshSubset.IndexBuffer);
    //glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, MeshSubset.IndexBuffer);
  end;
  glBindVertexArray(0);
end;

procedure TNode.TAttachmentSkin.UpdatePose;
  var i: Int32;
begin
  for i := 0 to High(_Pose) do
  begin
    _Pose[i] := (_Skin.Bind * _Skin.Joints[i].Bind * _JointBindings[i].Transform);
  end;
end;

procedure TNode.ChildAdd(const Child: TNode);
begin
  specialize UArrAppend<TNode>(_Children, Child);
end;

procedure TNode.ChildRemove(const Child: TNode);
begin
  specialize UArrRemove<TNode>(_Children, Child);
end;

procedure TNode.AttachAdd(const Attach: TAttachment);
begin
  specialize UArrAppend<TAttachment>(_Attachments, Attach);
end;

procedure TNode.AttachRemove(const Attach: TAttachment);
begin
  specialize UArrRemove<TAttachment>(_Attachments, Attach);
end;

procedure TNode.SetParent(const Value: TNode);
begin
  if _Parent = Value then Exit;
  if Assigned(_Parent) then _Parent.ChildRemove(Self);
  _Parent := Value;
  if Assigned(_Parent) then _Parent.ChildAdd(Self);
end;

procedure TNode.ApplyTransform(const Value: TUMat);
  var i: Int32;
begin
  _Transform := _Transform * Value;
  for i := 0 to High(_Children) do
  begin
    _Children[i].ApplyTransform(Value);
  end;
end;

procedure TNode.SetTransform(const Value: TUMat);
  var Diff: TUMat;
begin
  Diff := _Transform.Inverse * Value;
  ApplyTransform(Diff);
end;

function TNode.GetLocalTransform: TUMat;
begin
  if Assigned(_Parent) then
  begin
    Result := _Parent.Transform.Inverse * _Transform;
  end
  else
  begin
    Result := _Transform;
  end;
end;

procedure TNode.SetLocalTransform(const Value: TUMat);
begin
  if Assigned(_Parent) then
  begin
    SetTransform(Value * _Parent.Transform)
  end
  else
  begin
    SetTransform(Value);
  end;
end;

constructor TNode.Create(
  const AParent: TNode;
  const NodeData: TUSceneData.TNodeInterface
);
  var i: Int32;
begin
  _Name := NodeData.Name;
  Parent := AParent;
  Form1.NodeRemap.Add(NodeData, Self);
  _Transform := NodeData.Transform;
  for i := 0 to High(NodeData.Children) do
  begin
    TNode.Create(Self, NodeData.Children[i]);
  end;
end;

destructor TNode.Destroy;
begin
  specialize UArrClear<TNode>(_Children);
  inherited Destroy;
end;

procedure TNode.SetupAttachments(const NodeData: TUSceneData.TNodeInterface);
  var i: Int32;
  var AttachMesh: TAttachmentMesh;
  var AttachSkin: TAttachmentSkin;
begin
  for i := 0 to High(NodeData.Attachments) do
  begin
    if NodeData.Attachments[i] is TUSceneData.TAttachmentMesh then
    begin
      AttachMesh := TAttachmentMesh.Create(
        TUSceneData.TAttachmentMesh(NodeData.Attachments[i])
      );
      AttachMesh.Node := Self;
    end
    else if NodeData.Attachments[i] is TUSceneData.TAttachmentSkin then
    begin
      AttachSkin := TAttachmentSkin.Create(
        TUSceneData.TAttachmentSkin(NodeData.Attachments[i])
      );
      AttachSkin.Node := Self;
    end;
  end;
  for i := 0 to High(NodeData.Children) do
  begin
    Form1.NodeRemap.FindValueByKey(
      NodeData.Children[i]
    ).SetupAttachments(
      NodeData.Children[i]
    );
  end;
end;

procedure TNode.Setup;
  var Attach: TAttachment;
  var Node: TNode;
begin
  for Attach in _Attachments do
  begin
    Attach.Setup;
  end;
  for Node in _Children do
  begin
    Node.Setup;
  end;
end;

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

function TForm1.TF_Load(const Args: array of const): TNodeShared;
  var i: Integer;
  var Scene: TUSceneDataDAE;
  var FileName: String;
begin
  if Length(Args) < 1 then Exit(nil);
  FileName := AnsiString(Args[0].VAnsiString);
  if not FileExists(FileName) then Exit(nil);
  MakeCurrentShared;
  LoadDir := ExtractFileDir(FileName);
  Scene := TUSceneDataDAE.Create([sdo_optimize]);
  try
    Scene.Load(FileName);
    SetLength(Textures, Length(Scene.ImageList));
    for i := 0 to High(Textures) do
    begin
      Textures[i] := TTexture.Create(Scene.ImageList[i]);
      TextureRemap.Add(Scene.ImageList[i], Textures[i]);
    end;
    SetLength(Meshes, Length(Scene.MeshList));
    for i := 0 to High(Meshes) do
    begin
      Meshes[i] := TMesh.Create(Scene.MeshList[i]);
      MeshRemap.Add(Scene.MeshList[i], Meshes[i]);
    end;
    SetLength(Skins, Length(Scene.SkinList));
    for i := 0 to High(Skins) do
    begin
      Skins[i] := TSkin.Create(Scene.SkinList[i]);
      SkinRemap.Add(Scene.SkinList[i], Skins[i]);
    end;
    SetLength(Materials, Length(Scene.MaterialList));
    for i := 0 to High(Materials) do
    begin
      Materials[i] := TMaterial.Create(Scene.MaterialList[i]);
      MaterialRemap.Add(Scene.MaterialList[i], Materials[i]);
    end;
    Result := TNode.Create(nil, Scene.RootNode);
    Result.Ptr.SetupAttachments(Scene.RootNode);
    SetLength(Animations, Length(Scene.AnimationList));
    for i := 0 to High(Animations) do
    begin
      Animations[i] := TAnimation.Create(Scene.AnimationList[i]);
    end;
  finally
    FreeAndNil(Scene);
  end;
  glFlush();
end;

function TForm1.RequestDebugContext: Boolean;
begin
  //Result := False;
  Result := True;
end;

procedure TForm1.Initialize;
begin
  AppStartTime := GetTickCount64;
  TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('siren/siren_anim.dae')]);
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('Vanguard By T. Choonyung/Vanguard By T. Choonyung.dae')]);
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('Vampire A Lusth/Vampire A Lusth.dae')]);
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('X Bot.dae')]);
  //TF_Load([AssetsFile('X Bot.dae')]);
  Caption := 'PasOpenGL Loading...';
  //Load(AssetsFile('siren/siren_anim.dae'));
  //Load('../Assets/skin.dae');
  //Load('../Assets/box.dae');
end;

procedure TForm1.Finalize;
begin
  TextureRemap.Clear;
  MaterialRemap.Clear;
  SkinRemap.Clear;
  MeshRemap.Clear;
  NodeRemap.Clear;
  RootNode := nil;
  Skins := nil;
  Meshes := nil;
  Textures := nil;
  Materials := nil;
  Animations := nil;
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
  procedure DrawNode(const Node: TNode);
    var Attach: TNode.TAttachment;
    var AttachMesh: TNode.TAttachmentMesh;
    var AttachSkin: TNode.TAttachmentSkin;
    var NewBuffer, NewTexture: TGLuint;
    var NewShader: TShader;
    var Material: TMaterial;
    var ColorVec: TUVec4;
    var Xf: TUMat;
    var i: Int32;
  begin
    for Attach in Node.Attachments do
    if Attach is TNode.TAttachmentMesh then
    begin
      AttachMesh := TNode.TAttachmentMesh(Attach);
      Xf := AttachMesh.Node.Transform;
      WVP := Xf * W * V * P;
      for i := 0 to High(AttachMesh.Mesh.Subsets) do
      begin
        Material := AttachMesh.Materials[i];
        NewShader := AttachMesh.Shaders[i].Ptr;
        NewShader.Use;
        NewBuffer := AttachMesh.VertexArrays[i];
        glBindVertexArray(NewBuffer);
        if Material.Texture.IsValid
        and (NewShader.UniformTex0 >= 0) then
        begin
          NewTexture := Material.Texture.Ptr.Handle;
          glBindTextureUnit(0, NewTexture);
          //glActiveTexture(GL_TEXTURE0);
          //glBindTexture(GL_TEXTURE_2D, NewTexture);
          glUniform1i(NewShader.UniformTex0, 0);
        end;
        ColorVec := Material.Color;
        glUniform4fv(NewShader.UniformMatColor, 1, @ColorVec);
        glUniformMatrix4fv(NewShader.UniformWVP, 1, GL_TRUE, @WVP);
        AttachMesh.Mesh.DrawSubset(i);
      end;
    end
    else if Attach is TNode.TAttachmentSkin then
    begin
      AttachSkin := TNode.TAttachmentSkin(Attach);
      AttachSkin.UpdatePose;
      Xf := AttachSkin.Node.Transform;
      WVP := Xf * W * V * P;
      for i := 0 to High(AttachSkin.Skin.Subsets) do
      begin
        Material := AttachSkin.Materials[i];
        NewShader := AttachSkin.Shaders[i].Ptr;
        NewShader.Use;
        NewBuffer := AttachSkin.VertexArrays[i];
        glBindVertexArray(NewBuffer);
        if Material.Texture.IsValid
        and (NewShader.UniformTex0 >= 0) then
        begin
          NewTexture := Material.Texture.Ptr.Handle;
          glBindTextureUnit(0, NewTexture);
          //glActiveTexture(GL_TEXTURE0);
          //glBindTexture(GL_TEXTURE_2D, NewTexture);
          glUniform1i(NewShader.UniformTex0, 0);
        end;
        ColorVec := Material.Color;
        glUniform4fv(NewShader.UniformMatColor, 1, @ColorVec);
        glUniformMatrix4fv(NewShader.UniformWVP, 1, GL_TRUE, @WVP);
        glUniformMatrix4fv(NewShader.UniformBone, Length(AttachSkin.Pose), GL_TRUE, @AttachSkin.Pose[0]);
        AttachSkin.Skin.Mesh.DrawSubset(i);
      end;
    end;
    for i := 0 to High(Node.Children) do
    begin
      DrawNode(Node.Children[i]);
    end;
  end;
  procedure ApplyAnimation(const Animation: TAnimation; const Time: TUFloat);
    var i: Int32;
  begin
    for i := 0 to High(Animation.Tracks) do
    begin
      Animation.Tracks[i].Target.LocalTransform := Animation.Tracks[i].Sample(Time).Norm;
    end;
  end;
  var t: TUFloat;
  var i: Int32;
begin
  W := TUMat.Identity;
  //W := TUMat.Scaling(0.05);
  //W := W * TUMat.RotationX(UHalfPi);
  W := W * TUMat.RotationZ(((GetTickCount64 mod 4000) / 4000) * UTwoPi);
  v := TUMat.View(TUVec3.Make(10, 10, 10), TUVec3.Make(0, 0, 5), TUVec3.Make(0, 0, 1));
  P := TUMat.Proj(UPi * 0.3, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  t := (GetTickCount64 - AppStartTime) * 0.001;
  for i := 0 to High(Animations) do
  begin
    ApplyAnimation(Animations[i].Ptr, t * 3);
  end;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.4, 1, 0.8, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if RootNode.IsValid then
  begin
    DrawNode(RootNode.Ptr);
    glBindVertexArray(0);
  end
  else if TaskLoad.IsComplete then
  begin
    RootNode := TaskLoad.TaskResult;
    TaskLoad.Reset;
    RootNode.Ptr.Setup;
    PrintScene;
    Caption := 'PasOpenGL';
  end;
end;

procedure TForm1.PrintScene;
  procedure PrintNode(const Node: TNode; const Offset: String = '');
    var i: Int32;
  begin
    WriteLn(Offset, Node.Name, ' (', Node.ClassName, ')');
    for i := 0 to High(Node.Attachments) do
    begin
      WriteLn(Offset, Node.Attachments[i].ClassName);
    end;
    for i := 0 to High(Node.Children) do
    begin
      PrintNode(Node.Children[i], Offset + '  ');
    end;
  end;
begin
  PrintNode(RootNode.Ptr);
end;

end.

