unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$warn 6058 off}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, PasOpenGL,
  Math, CommonUtils, MediaUtils, Setup;

type TGLuintArray = array of TGLuint;
type TLoadingContext = class;
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
  class var _ShaderMap: specialize TUMap<UInt64, TShader>;
public
  property Handle: TGLuint read _Handle;
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
  constructor Create(
    constref Context: TLoadingContext;
    const SkinData: TUSceneData.TSkinInterface
  );
  destructor Destroy; override;
end;
type TSkinShared = specialize TUSharedRef<TSkin>;

type TTexture = class (TURefClass)
private
  var _Handle: TGLuint;
public
  property Handle: TGLuint read _Handle;
  constructor Create(
    const Context: TLoadingContext;
    const ImageData: TUSceneData.TImageInterface
  );
  destructor Destroy; override;
end;
type TTextureShared = specialize TUSharedRef<TTexture>;

type TMaterial = class (TURefClass)
public
  type TMaterialList = array of TMaterial;
private
  var _AmbientTexture: TTextureShared;
  var _AmbientColor: TUColor;
  var _DiffuseTexture: TTextureShared;
  var _DiffuseColor: TUColor;
  var _EmissiveTexture: TTextureShared;
  var _EmissiveColor: TUColor;
  var _SpecularTexture: TTextureShared;
  var _SpecularColor: TUColor;
  var _NormalTexture: TTextureShared;
  var _SpecularPower: TUFloat;
public
  property AmbientTexture: TTextureShared read _AmbientTexture;
  property AmbientColor: TUColor read _AmbientColor;
  property DiffuseTexture: TTextureShared read _DiffuseTexture;
  property DiffuseColor: TUColor read _DiffuseColor;
  property EmissiveTexture: TTextureShared read _EmissiveTexture;
  property EmissiveColor: TUColor read _EmissiveColor;
  property SpecularTexture: TTextureShared read _SpecularTexture;
  property SpecularColor: TUColor read _SpecularColor;
  property NormalTexture: TTextureShared read _NormalTexture;
  property SpecularPower: TUFloat read _SpecularPower;
  constructor Create(
    const Context: TLoadingContext;
    const MaterialData: TUSceneData.TMaterialInterface
  );
  destructor Destroy; override;
end;
type TMaterialShared = specialize TUSharedRef<TMaterial>;

type TAnimation = class (TURefClass)
public
  type TTrack = class
  public
    type TKey = record
      var Time: TUFloat;
      var Value: TUTransform;
      var Interpolation: TUSceneData.TAnimationInterface.TKeyInterpolation;
    end;
    type TKeyList = array of TKey;
  private
    var _Name: String;
    var _Keys: TKeyList;
    var _Target: String;
    var _MaxTime: TUFloat;
    function FindKey(const Time: TUFloat): Int32;
  public
    property Name: String read _Name;
    property Keys: TKeyList read _Keys;
    property Target: String read _Target;
    property MaxTime: TUFloat read _MaxTime;
    function Sample(const Time: TUFloat; const Loop: Boolean = True): TUTransform;
    constructor Create(
      const TrackData: TUSceneData.TAnimationInterface.TTrack
    );
    destructor Destroy; override;
  end;
  type TTrackList = array of TTrack;
private
  var _Retarget: String;
  var _Tracks: TTrackList;
public
  property Retarget: String read _Retarget;
  property Tracks: TTrackList read _Tracks;
  constructor Create(
    const AnimationData: TUSceneData.TAnimationInterfaceList
  );
  destructor Destroy; override;
end;
type TAnimationShared = specialize TUSharedRef<TAnimation>;

type TAnimationInstance = class (TURefClass)
public
  type TTrack = class
  public
    var Track: TAnimation.TTrack;
    var Target: TNode;
    constructor Create(const ATrack: TAnimation.TTrack; const ATarget: TNode);
  end;
  type TTrackList = array of TTrack;
private
  var _Tracks: TTrackList;
public
  property Tracks: TTrackList read _Tracks;
  constructor Create(
    const Animation: TAnimation;
    const TargetNode: TNode;
    const Retarget: String = ''
  );
  destructor Destroy; override;
  procedure Apply(const Time: TUFloat);
end;
type TAnimationInstanceShared = specialize TUSharedRef<TAnimationInstance>;

type TAnimationBlend = class (TURefClass)
private
  type TBlendable = record
    Instance: TAnimationInstanceShared;
    Remap: array of Int32;
    Weight: TUFloat;
    Time: TUFloat;
  end;
  type TBlendableList = array of TBlendable;
  type TBlendableTarget = record
    Node: TNode;
    Weight: TUFloat;
    Xf: TUTransform;
  end;
  var _Blendables: TBlendableList;
  var _Targets: array of TBlendableTarget;
  function FindBlendable(const Animation: TAnimationInstance): Int32;
  function GetBlendableCount: Int32;
public
  property BlendableCount: Int32 read GetBlendableCount;
  constructor Create(const Animations: array of TAnimationInstanceShared);
  function GetWeight(const BlendableIndex: Int32): TUFloat;
  function GetWeight(const Animation: TAnimationInstance): TUFloat;
  procedure SetWeight(const BlendableIndex: Int32; const Value: TUFloat);
  procedure SetWeight(const Animation: TAnimationInstance; const Value: TUFloat);
  function GetTime(const BlendableIndex: Int32): TUFloat;
  function GetTime(const Animation: TAnimationInstance): TUFloat;
  procedure SetTime(const BlendableIndex: Int32; const Value: TUFloat);
  procedure SetTime(const Animation: TAnimationInstance; const Value: TUFloat);
  procedure SetTime(const Value: TUFloat);
  procedure Apply;
  procedure Apply(const Time: TUFloat);
end;
type TAnimationBlendShared = specialize TUSharedRef<TAnimationBlend>;

type TUniformGroup = record
  ViewPos: TGLint;
  W: TGLint;
  WVP: TGLint;
  Bone: TGLint;
  MatAmbientColor: TGLint;
  MatAmbientTexture: TGLint;
  MatDiffuseColor: TGLint;
  MatDiffuseTexture: TGLint;
  MatEmissiveColor: TGLint;
  MatEmissiveTexture: TGLint;
  MatSpecularColor: TGLint;
  MatSpecularTexture: TGLint;
  MatNormalTexture: TGLint;
  procedure Setup(const Shader: TShader);
end;
type TUniformGroupList = array of TUniformGroup;

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
    var _Uniforms: TUniformGroupList;
    var _VertexArrays: TGLuintArray;
  public
    property Mesh: TMesh read _Mesh;
    property Materials: TMaterial.TMaterialList read _Materials;
    property Shaders: TShaderList read _Shaders;
    property Uniforms: TUniformGroupList read _Uniforms;
    property VertexArrays: TGLuintArray read _VertexArrays;
    constructor Create(
      const Context: TLoadingContext;
      const AttachData: TUSceneData.TAttachmentMesh
    );
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
    var _Uniforms: TUniformGroupList;
    var _VertexArrays: TGLuintArray;
    var _Pose: TUMatArray;
    var _JointBindings: TNodeList;
  public
    property Skin: TSkin read _Skin;
    property Materials: TMaterial.TMaterialList read _Materials;
    property Shaders: TShaderList read _Shaders;
    property Uniforms: TUniformGroupList read _Uniforms;
    property VertexArrays: TGLuintArray read _VertexArrays;
    property Pose: TUMatArray read _Pose;
    constructor Create(
      const Context: TLoadingContext;
      const AttachData: TUSceneData.TAttachmentSkin
    );
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
  var _Ids: TUStrArray;
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
  property Ids: TUStrArray read _Ids;
  constructor Create(
    const Context: TLoadingContext;
    const AParent: TNode;
    const NodeData: TUSceneData.TNodeInterface
  );
  destructor Destroy; override;
  procedure SetupAttachments(
    const Context: TLoadingContext;
    const NodeData: TUSceneData.TNodeInterface
  );
  procedure Setup;
  function MatchId(const Id: String): Boolean;
end;
type TNodeShared = specialize TUSharedRef<TNode>;

type TLoadingContext = class
public
  var TextureRemap: specialize TUMap<Pointer, TTextureShared>;
  var MeshRemap: specialize TUMap<Pointer, TMeshShared>;
  var SkinRemap: specialize TUMap<Pointer, TSkinShared>;
  var MaterialRemap: specialize TUMap<Pointer, TMaterialShared>;
  var NodeRemap: specialize TUMap<Pointer, TNode>;
  var LoadDir: String;
  constructor Create(const FilePath: String);
end;

type TScene = class (TURefClass)
public
  var Meshes: array of TMeshShared;
  var Skins: array of TSkinShared;
  var Textures: array of TTextureShared;
  var Materials: array of TMaterialShared;
  var Animation: TAnimationShared;
  var RootNode: TNodeShared;
end;
type TSceneShared = specialize TUSharedRef<TScene>;
type TSceneList = array of TSceneShared;

type TForm1 = class(TCommonForm)
private
  var Scenes: TSceneList;
  var RootNode: TNodeShared;
  var AnimInstance: TAnimationInstanceShared;
  var AnimBlend: TAnimationBlendShared;
  var AppStartTime: UInt64;
  var TaskLoad: specialize TUTask<TSceneList>;
  var Anims: array [0..1] of Int32;
  var AnimTime: TUFloat;
  procedure ImageFormatToGL(const ImageFormat: TUImageDataFormat; out Format, DataType: TGLenum);
  function TF_Load(const Args: array of const): TSceneList;
protected
  function RequestDebugContext: Boolean; override;
public
  procedure Initialize; override;
  procedure Finalize; override;
  procedure Tick; override;
  procedure SetupScenes;
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
  var vs, ps, Inputs, Outputs, AttName, AttSize, AttType: String;
  var i, BoneCount, AttribIndex: Int32;
  var PositionId, TexCoordId, NormalId, TangentId, BinormalId: Int32;
  var PositionName, TexCoordName, NormalName, TangentName, BinormalName: String;
begin
  Hash := MakeHash;
  Result := _ShaderMap.FindValueByKey(Hash);
  if Assigned(Result) then Exit;
  vs := '#version 450 core'#$D#$A;
  Inputs := '';
  Outputs := '';
  PositionId := -1;
  TexCoordId := -1;
  NormalId := -1;
  TangentId := -1;
  BinormalId := -1;
  AttribIndex := 0;
  for Attrib in VertexDescriptor do
  begin
    AttName := AttributeName(Attrib);
    AttSize := IntToStr(Attrib.DataCount);
    if Attrib.DataCount = 1 then AttType := 'float' else AttType := 'vec' + AttSize;
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in ' + AttType + ' in_' + AttName + ';'#$D#$A;
    Outputs += 'layout (location = ' + IntToStr(AttribIndex) + ') out ' + AttType + ' out_' + AttName + ';'#$D#$A;
    if (PositionId = -1)
    and (Attrib.Semantic = as_position) then
    begin
      PositionId := AttribIndex;
      PositionName := AttName;
    end
    else if (TexCoordId = -1)
    and (Attrib.Semantic = as_texcoord) then
    begin
      TexCoordId := AttribIndex;
      TexCoordName := AttName;
    end
    else if (NormalId = -1)
    and (Attrib.Semantic = as_normal) then
    begin
      NormalId := AttribIndex;
      NormalName := AttName;
    end
    else if (TangentId = -1)
    and (Attrib.Semantic = as_tangent) then
    begin
      TangentId := AttribIndex;
      NormalName := AttName;
    end
    else if (BinormalId = -1)
    and (Attrib.Semantic = as_binormal) then
    begin
      BinormalId := AttribIndex;
      NormalName := AttName;
    end;
    Inc(AttribIndex);
  end;
  if Assigned(SkinInfo) then
  begin
    AttSize := IntToStr(SkinInfo^.BoneWeights);
    if SkinInfo^.BoneWeights = 1 then AttType := 'uint' else AttType := 'uvec' + AttSize;
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in ' + AttType + ' in_bone_index;'#$D#$A;
    Inc(AttribIndex);
    if SkinInfo^.BoneWeights = 1 then AttType := 'float' else AttType := 'vec' + AttSize;
    Inputs += 'layout (location = ' + IntToStr(AttribIndex) + ') in ' + AttType + ' in_bone_weight;'#$D#$A;
    Inc(AttribIndex);
  end;
  vs += Inputs + Outputs;
  vs += 'uniform mat4x4 W;'#$D#$A;
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
      if SkinInfo^.BoneWeights = 1 then AttType := '' else AttType := '[' + IntToStr(i) + ']';
      vs += '    (Bone[in_bone_index' + AttType + '] * in_bone_weight' + AttType + ')';
      if i < SkinInfo^.BoneWeights - 1 then vs += ' + ';
      vs += #$D#$A;
    end;
    vs += '  );'#$D#$A;
  end;
  for i := 0 to High(VertexDescriptor) do
  begin
    AttName := AttributeName(VertexDescriptor[i]);
    case VertexDescriptor[i].Semantic of
      as_position:
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
        vs += '  out_position = (position.xyz * mat4x3(W)).xyz;'#$D#$A;
      end;
      as_normal,
      as_tangent,
      as_binormal:
      begin
        if Assigned(SkinInfo) then
        begin
          vs += '  vec3 ' + AttName + ' = in_' + AttName + ' * mat3x3(S);'#$D#$A;
        end
        else
        begin
          vs += '  vec3 ' + AttName + ' = in_' + AttName + ';'#$D#$A;
        end;
        vs += '  out_' + AttName + ' = normalize(' + AttName + ' * mat3x3(W));'#$D#$A;
      end;
      as_texcoord:
      begin
        vs += '  out_' + AttName + ' = vec2(in_' + AttName + '.x, ' + 'in_' + AttName + '.y);'#$D#$A;
      end
      else
      begin
        vs += '  out_' + AttName + ' = in_' + AttName + ';'#$D#$A;
      end;
    end;
  end;
  vs += '}'#$D#$A;
  ps := '#version 450 core'#$D#$A;
  for i := 0 to High(VertexDescriptor) do
  begin
    AttName := AttributeName(VertexDescriptor[i]);
    AttSize := IntToStr(VertexDescriptor[i].DataCount);
    ps += 'layout (location = ' + IntToStr(i) + ') in vec' + AttSize + ' in_' + AttName + ';'#$D#$A;
  end;
  ps += 'out vec4 out_color;'#$D#$A;
  if Material.DiffuseTexture.IsValid then
  begin
    ps += 'uniform sampler2D m_diffuse_tex;'#$D#$A;
  end;
  if Material.AmbientTexture.IsValid then
  begin
    ps += 'uniform sampler2D m_ambient_tex;'#$D#$A;
  end;
  if Material.EmissiveTexture.IsValid then
  begin
    ps += 'uniform sampler2D m_emissive_tex;'#$D#$A;
  end;
  if Material.SpecularTexture.IsValid then
  begin
    ps += 'uniform sampler2D m_specular_tex;'#$D#$A;
  end;
  if Material.NormalTexture.IsValid then
  begin
    ps += 'uniform sampler2D m_normal_tex;'#$D#$A;
  end;
  ps += 'uniform vec4 m_ambient_col = vec4(1, 1, 1, 1);'#$D#$A;
  ps += 'uniform vec4 m_diffuse_col = vec4(1, 1, 1, 1);'#$D#$A;
  ps += 'uniform vec4 m_emissive_col = vec4(0, 0, 0, 1);'#$D#$A;
  if not Material.SpecularTexture.IsValid then
  begin
    ps += 'uniform vec4 m_specular_col = vec4(1, 1, 1, 0.5);'#$D#$A;
  end;
  ps += 'uniform vec3 light_dir = vec3(0, 1, 0);'#$D#$A;
  ps += 'uniform vec3 light_diffuse = vec3(1, 1, 1);'#$D#$A;
  ps += 'uniform vec3 light_ambient = vec3(0.5, 0.5, 0.5);'#$D#$A;
  ps += 'uniform vec3 light_specular = vec3(1, 1, 1);'#$D#$A;
  ps += 'uniform vec3 view_pos;'#$D#$A;
  ps += 'void main() {'#$D#$A;
  ps += '  vec4 m_diffuse = m_diffuse_col;'#$D#$A;
  if Material.DiffuseTexture.IsValid
  and (TexCoordId > -1) then
  begin
    ps += '  m_diffuse *= texture(m_diffuse_tex, in_' + TexCoordName + '.xy);'#$D#$A;
  end;
  ps += '  vec4 m_ambient = m_ambient_col;'#$D#$A;
  if Material.AmbientTexture.IsValid
  and (TexCoordId > -1) then
  begin
    ps += '  m_ambient += texture(m_ambient_tex, in_' + TexCoordName + '.xy);'#$D#$A;
  end;
  ps += '  vec4 m_emissive = m_emissive_col;'#$D#$A;
  if Material.AmbientTexture.IsValid
  and (TexCoordId > -1) then
  begin
    ps += '  m_emissive += texture(m_emissive_tex, in_' + TexCoordName + '.xy);'#$D#$A;
  end;
  if Material.SpecularTexture.IsValid
  and (TexCoordId > -1) then
  begin
    ps += '  vec4 m_specular = texture(m_specular_tex, in_' + TexCoordName + '.xy);'#$D#$A;
  end
  else
  begin
    ps += '  vec4 m_specular = m_specular_col;'#$D#$A;
  end;
  if NormalId > -1 then
  begin
    if (TangentId > -1)
    and (BinormalId > -1)
    and Material.NormalTexture.IsValid then
    begin
      ps += '  mat3x3 T = mat3x3(in_tangent.xyz, in_binormal.xyz, in_normal.xyz);'#$D#$A;
      ps += '  vec3 normal = texture(m_normal_tex, in_' + TexCoordName + '.xy).xyz;'#$D#$A;
      ps += '  normal = normalize((normal * 2.0 - 1.0) * T);'#$D#$A;
    end
    else
    begin
      ps += '  vec3 normal = in_normal;'#$D#$A;
    end;
    ps += '  vec3 view_dir = normalize(view_pos - in_position);'#$D#$A;
    ps += '  vec3 ref_dir = normalize(reflect(-light_dir, normal));'#$D#$A;
    ps += '  vec3 light_s = pow(max(dot(normal, ref_dir), 0.0), 4.0) * light_specular;'#$D#$A;
    ps += '  vec3 light_d = max(dot(normal, light_dir) * 0.5 + 0.5, 0.0) * light_diffuse;'#$D#$A;
    ps += '  light_d += m_ambient.xyz * light_ambient.xyz;'#$D#$A;
    ps += '  vec3 color = m_diffuse.xyz * light_d + m_specular.xyz * light_s + m_emissive.xyz;'#$D#$A;
    ps += '  out_color = vec4(color, m_diffuse.w);'#$D#$A;
    //ps += '  out_color = vec4(in_binormal, 1);'#$D#$A;
  end
  else
  begin
    ps += '  out_color = m_diffuse;'#$D#$A;
  end;
  ps += '}'#$D#$A;
  UStrToFile('vs_' + IntToHex(Hash) + '.txt', vs);
  UStrToFile('ps_' + IntToHex(Hash) + '.txt', ps);
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
  glCreateBuffers(1, @VertexBuffer);
  glNamedBufferStorage(
    VertexBuffer, VertexCount * VertexSize,
    SkinSubset.VertexData, 0
  );
end;

destructor TSkin.TSubset.Destroy;
begin
  glDeleteBuffers(1, @VertexBuffer);
  inherited Destroy;
end;

constructor TSkin.Create(constref Context: TLoadingContext; const SkinData: TUSceneData.TSkinInterface);
  var SubsetId, JointId: Int32;
begin
  _Mesh := Context.MeshRemap.FindValueByKey(SkinData.Mesh).Ptr;
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

constructor TTexture.Create(
  const Context: TLoadingContext;
  const ImageData: TUSceneData.TImageInterface
);
  var Image: TUImageDataShared;
  var TextureFormat, TextureType: TGLenum;
  var MipLevels: UInt32;
begin
  Image := ULoadImageData(Context.LoadDir + '/' + ImageData.FileName);
  if not Image.IsValid then
  begin
    _Handle := 0;
    Exit;
  end;
  MipLevels := Round(Math.Log2(UMax(Image.Ptr.Width, Image.Ptr.Height)));
  Form1.ImageFormatToGL(Image.Ptr.Format, TextureFormat, TextureType);
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
end;

destructor TTexture.Destroy;
begin
  if _Handle > 0 then glDeleteTextures(1, @_Handle);
  inherited Destroy;
end;

constructor TMaterial.Create(
  const Context: TLoadingContext;
  const MaterialData: TUSceneData.TMaterialInterface
);
  var i, j: Int32;
  var ParamName: String;
  var Param: TUSceneData.TMaterialInterface.TParam;
  var ParamFloat: TUSceneData.TMaterialInterface.TParamFloat absolute Param;
  var ParamVec4: TUSceneData.TMaterialInterface.TParamVec4 absolute Param;
  var Image: TUSceneData.TMaterialInterface.TParamImage absolute Param;
begin
  _AmbientColor := TUColor.Black;
  _DiffuseColor := TUColor.White;
  _EmissiveColor := TUColor.Black;
  _SpecularColor := TUColor.Black;
  _SpecularPower := 50;
  for i := 0 to High(MaterialData.Params) do
  begin
    Param := MaterialData.Params[i];
    ParamName := LowerCase(Param.Name);
    WriteLn(ParamName);
    if ParamName = 'emission' then
    begin
      if Param is TUSceneData.TMaterialInterface.TParamVec4 then
      begin
        _EmissiveColor := ParamVec4.Value;
      end
      else if Param is TUSceneData.TMaterialInterface.TParamImage then
      begin
        j := Context.TextureRemap.FindIndexByKey(Image.Image);
        if j > -1 then _EmissiveTexture := Context.TextureRemap[j];
      end;
    end
    else if ParamName = 'ambient' then
    begin
      if Param is TUSceneData.TMaterialInterface.TParamVec4 then
      begin
        _AmbientColor := ParamVec4.Value;
      end
      else if Param is TUSceneData.TMaterialInterface.TParamImage then
      begin
        j := Context.TextureRemap.FindIndexByKey(Image.Image);
        if j > -1 then _AmbientTexture := Context.TextureRemap[j];
      end;
    end
    else if ParamName = 'diffuse' then
    begin
      if Param is TUSceneData.TMaterialInterface.TParamVec4 then
      begin
        _DiffuseColor := ParamVec4.Value;
      end
      else if Param is TUSceneData.TMaterialInterface.TParamImage then
      begin
        j := Context.TextureRemap.FindIndexByKey(Image.Image);
        if j > -1 then _DiffuseTexture := Context.TextureRemap[j];
      end;
    end
    else if ParamName = 'specular' then
    begin
      if Param is TUSceneData.TMaterialInterface.TParamVec4 then
      begin
        _SpecularColor := ParamVec4.Value;
      end
      else if Param is TUSceneData.TMaterialInterface.TParamImage then
      begin
        j := Context.TextureRemap.FindIndexByKey(Image.Image);
        if j > -1 then _SpecularTexture := Context.TextureRemap[j];
      end;
    end
    else if (ParamName = 'displacement')
    or (ParamName = 'normal')
    or (ParamName = 'bump') then
    begin
      if Param is TUSceneData.TMaterialInterface.TParamImage then
      begin
        j := Context.TextureRemap.FindIndexByKey(Image.Image);
        if j > -1 then _NormalTexture := Context.TextureRemap[j];
      end;
    end
    else if (ParamName = 'shininess') then
    begin
      _SpecularPower := ParamFloat.Value;
    end;
  end;
end;

destructor TMaterial.Destroy;
begin
  inherited Destroy;
end;

constructor TAnimation.Create(
  const AnimationData: TUSceneData.TAnimationInterfaceList
);
  var i, j: Int32;
begin
  for i := 0 to High(AnimationData) do
  for j := 0 to High(AnimationData[i].Tracks) do
  begin
    specialize UArrAppend<TTrack>(
      _Tracks, TTrack.Create(AnimationData[i].Tracks[j])
    );
  end;
  if Length(_Tracks) < 1 then Exit;
  _Retarget := _Tracks[0].Target;
  for i := 1 to High(_Tracks) do
  begin
    for j := 1 to UMin(Length(_Retarget), Length(_Tracks[i].Target)) do
    begin
      if _Retarget[j] <> _Tracks[i].Target[j] then Break;
    end;
    if j <> Length(_Retarget) then
    begin
      SetLength(_Retarget, j - 1);
    end;
  end;
end;

destructor TAnimation.Destroy;
begin
  specialize UArrClear<TTrack>(_Tracks);
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

function TAnimation.TTrack.Sample(const Time: TUFloat; const Loop: Boolean): TUTransform;
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
      t := UClamp((t - _Keys[k0].Time) / (_Keys[k1].Time - _Keys[k0].Time), 0, 1);
      Result := TUTransform.Interpolate(_Keys[k0].Value, _Keys[k1].Value, t);
    end;
  end;
end;

constructor TAnimation.TTrack.Create(
  const TrackData: TUSceneData.TAnimationInterface.TTrack
);
  var i: Int32;
begin
  _Target := TrackData.TargetId;
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

constructor TAnimationInstance.TTrack.Create(
  const ATrack: TAnimation.TTrack;
  const ATarget: TNode
);
begin
  Track := ATrack;
  Target := ATarget;
end;

function TAnimationBlend.FindBlendable(const Animation: TAnimationInstance): Int32;
  var i: Int32;
begin
  for i := 0 to High(_Blendables) do
  if _Blendables[i].Instance.Ptr = Animation then Exit(i);
  Result := -1;
end;

function TAnimationBlend.GetBlendableCount: Int32;
begin
  Result := Length(_Blendables);
end;

constructor TAnimationBlend.Create(
  const Animations: array of TAnimationInstanceShared
);
  function AddTarget(const Node: TNode): Int32;
    var i: Int32;
    var t: TBlendableTarget;
  begin
    for i := 0 to High(_Targets) do
    if _Targets[i].Node = Node then Exit(i);
    t.Node := Node;
    t.Weight := 0;
    t.Xf := TUMat.Zero;
    Result := specialize UArrAppend<TBlendableTarget>(_Targets, t);
  end;
  var i, j, r: Int32;
begin
  SetLength(_Blendables, Length(Animations));
  for i := 0 to High(_Blendables) do
  begin
    _Blendables[i].Instance := Animations[i];
    if i = 0 then _Blendables[i].Weight := 1
    else _Blendables[i].Weight := 0;
    SetLength(_Blendables[i].Remap, Length(Animations[i].Ptr.Tracks));
  end;
  if Length(_Blendables) < 1 then Exit;
  SetLength(_Targets, Length(_Blendables[0].Remap));
  for j := 0 to High(_Blendables[0].Instance.Ptr.Tracks) do
  begin
    _Targets[j].Node := _Blendables[0].Instance.Ptr.Tracks[j].Target;
    _Targets[j].Weight := 0;
    _Targets[j].Xf := TUMat.Zero;
    _Blendables[0].Remap[j] := j;
  end;
  for i := 1 to High(_Blendables) do
  begin
    for j := 0 to High(_Blendables[i].Instance.Ptr.Tracks) do
    begin
      _Blendables[i].Remap[j] := AddTarget(
        _Blendables[i].Instance.Ptr.Tracks[j].Target
      );
    end;
  end;
end;

function TAnimationBlend.GetWeight(const BlendableIndex: Int32): TUFloat;
begin
  Result := _Blendables[BlendableIndex].Weight;
end;

function TAnimationBlend.GetWeight(const Animation: TAnimationInstance): TUFloat;
  var i: Int32;
begin
  i := FindBlendable(Animation);
  if i = -1 then Exit(0);
  Result := _Blendables[i].Weight;
end;

procedure TAnimationBlend.SetWeight(const BlendableIndex: Int32; const Value: TUFloat);
begin
  _Blendables[BlendableIndex].Weight := Value;
end;

procedure TAnimationBlend.SetWeight(const Animation: TAnimationInstance; const Value: TUFloat);
  var i: Int32;
begin
  i := FindBlendable(Animation);
  if i = -1 then Exit;
  _Blendables[i].Weight := Value;
end;

function TAnimationBlend.GetTime(const BlendableIndex: Int32): TUFloat;
begin
  Result := _Blendables[BlendableIndex].Time;
end;

function TAnimationBlend.GetTime(const Animation: TAnimationInstance): TUFloat;
  var i: Int32;
begin
  i := FindBlendable(Animation);
  if i = -1 then Exit(0);
  Result := _Blendables[i].Time;
end;

procedure TAnimationBlend.SetTime(const BlendableIndex: Int32; const Value: TUFloat);
begin
  _Blendables[BlendableIndex].Time := Value;
end;

procedure TAnimationBlend.SetTime(const Animation: TAnimationInstance; const Value: TUFloat);
  var i: Int32;
begin
  i := FindBlendable(Animation);
  if i = -1 then Exit;
  _Blendables[i].Time := Value;
end;

procedure TAnimationBlend.SetTime(const Value: TUFloat);
  var i: Int32;
begin
  for i := 0 to High(_Blendables) do
  begin
    _Blendables[i].Time := Value;
  end;
end;

procedure TAnimationBlend.Apply;
  var i, j, r: Int32;
  var w, tw: TUFloat;
  var Xf0, Xf1: TUTransform;
begin
  for i := 0 to High(_Targets) do
  begin
    _Targets[i].Xf := TUTransform.Identity;
    _Targets[i].Weight := 0;
  end;
  for i := 0 to High(_Blendables) do
  begin
    if _Blendables[i].Weight < UEps then Continue;
    for j := 0 to High(_Blendables[i].Instance.Ptr.Tracks) do
    begin
      Xf0 := _Blendables[i].Instance.Ptr.Tracks[j].Track.Sample(_Blendables[i].Time);
      r := _Blendables[i].Remap[j];
      w := _Targets[r].Weight;
      if (w > UEps) then
      begin
        Xf1 := _Targets[r].Xf;
        tw := w + _Blendables[i].Weight;
        w := w / tw;
        _Targets[r].Xf := TUTransform.Interpolate(Xf0, Xf1, w);
        _Targets[r].Weight := tw;
      end
      else
      begin
        _Targets[r].Xf := Xf0;
        _Targets[r].Weight := _Blendables[i].Weight;
      end;
    end;
  end;
  for i := 0 to High(_Targets) do
  begin
    if _Targets[i].Weight < UEps then Continue;
    _Targets[i].Node.LocalTransform := _Targets[i].Xf;
  end;
end;

procedure TAnimationBlend.Apply(const Time: TUFloat);
begin
  SetTime(Time);
  Apply;
end;

procedure TUniformGroup.Setup(const Shader: TShader);
begin
  ViewPos := Shader.UniformLocation('view_pos');
  W := Shader.UniformLocation('W');
  WVP := Shader.UniformLocation('WVP');
  Bone := Shader.UniformLocation('Bone');
  MatAmbientColor := Shader.UniformLocation('m_ambient_col');
  MatAmbientTexture := Shader.UniformLocation('m_ambient_tex');
  MatDiffuseColor := Shader.UniformLocation('m_diffuse_col');
  MatDiffuseTexture := Shader.UniformLocation('m_diffuse_tex');
  MatEmissiveColor := Shader.UniformLocation('m_emissive_col');
  MatEmissiveTexture := Shader.UniformLocation('m_emissive_tex');
  MatSpecularColor := Shader.UniformLocation('m_specular_col');
  MatSpecularTexture := Shader.UniformLocation('m_specular_tex');
  MatNormalTexture := Shader.UniformLocation('m_normal_tex');
end;

constructor TAnimationInstance.Create(
  const Animation: TAnimation;
  const TargetNode: TNode;
  const Retarget: String
);
  function FindNode(const Node: TNode; const Id: String): TNode;
    var i: Int32;
    var IdRet: String;
  begin
    if Length(Retarget) > 0 then
    begin
      IdRet := Id.Replace(Animation.Retarget, Retarget);
      if Node.MatchId(IdRet) then Exit(Node);
    end;
    if Node.MatchId(Id) then Exit(Node);
    for i := 0 to High(Node.Children) do
    begin
      Result := FindNode(Node.Children[i], Id);
      if Assigned(Result) then Exit;
    end;
    Result := nil;
  end;
  var i: Int32;
  var Node: TNode;
begin
  for i := 0 to High(Animation.Tracks) do
  begin
    Node := FindNode(TargetNode, Animation.Tracks[i].Target);
    if not Assigned(Node) then Continue;
    specialize UArrAppend<TTrack>(
      _Tracks,
      TTrack.Create(Animation.Tracks[i], Node)
    );
  end;
end;

destructor TAnimationInstance.Destroy;
begin
  specialize UArrClear<TTrack>(_Tracks);
  inherited Destroy;
end;

procedure TAnimationInstance.Apply(const Time: TUFloat);
  var i: Int32;
  var Xf: TUMat;
begin
  for i := 0 to High(_Tracks) do
  begin
    Xf := _Tracks[i].Track.Sample(Time);
    _Tracks[i].Target.LocalTransform := Xf;
  end;
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
  const Context: TLoadingContext;
  const AttachData: TUSceneData.TAttachmentMesh
);
  var i: Int32;
begin
  inherited Create;
  _Mesh := Context.MeshRemap.FindValueByKey(AttachData.Mesh).Ptr;
  SetLength(_Materials, Length(AttachData.MaterialBindings));
  for i := 0 to High(_Materials) do
  begin
    if Assigned(AttachData.MaterialBindings[i].BaseMaterial) then
    begin
      _Materials[i] := Context.MaterialRemap.FindValueByKey(
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
  var vd: TUVertexDescriptor;
  var VertexArray: TGLuint;
begin
  if Length(_VertexArrays) > 0 then Exit;
  SetLength(_VertexArrays, Length(_Mesh.Subsets));
  SetLength(_Shaders, Length(_Mesh.Subsets));
  SetLength(_Uniforms, Length(_Mesh.Subsets));
  glCreateVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  for SubsetId := 0 to High(_Mesh.Subsets) do
  begin
    VertexArray := _VertexArrays[SubsetId];
    Subset := _Mesh.Subsets[SubsetId];
    vd := Subset.VertexDescriptor;
    _Shaders[SubsetId] := TShader.AutoShader(vd, _Materials[SubsetId]);
    _Uniforms[SubsetId].Setup(_Shaders[SubsetId].Ptr);
    glVertexArrayVertexBuffer(
      VertexArray,
      0, Subset.VertexBuffer,
      0, Subset.VertexSize
    );
    AttribOffset := 0;
    for i := 0 to High(vd) do
    begin
      glEnableVertexArrayAttrib(VertexArray, i);
      glVertexArrayAttribFormat(
        VertexArray, i, vd[i].DataCount,
        GL_FLOAT, GL_FALSE, AttribOffset
      );
      glVertexArrayAttribBinding(VertexArray, i, 0);
      AttribOffset += vd[i].Size;
    end;
    glVertexArrayElementBuffer(
      VertexArray,
      Subset.IndexBuffer
    );
  end;
  glBindVertexArray(0);
end;

constructor TNode.TAttachmentSkin.Create(
  const Context: TLoadingContext;
  const AttachData: TUSceneData.TAttachmentSkin
);
  var i: Int32;
begin
  inherited Create;
  _Skin := Context.SkinRemap.FindValueByKey(AttachData.Skin).Ptr;
  SetLength(_Materials, Length(AttachData.MaterialBindings));
  for i := 0 to High(_Materials) do
  begin
    if Assigned(AttachData.MaterialBindings[i].BaseMaterial) then
    begin
      _Materials[i] := Context.MaterialRemap.FindValueByKey(
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
    _JointBindings[i] := Context.NodeRemap.FindValueByKey(AttachData.JointBindings[i]);
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
  var AttribOffset: GLuint;
  var vd: TUVertexDescriptor;
  var VertexArray: TGLuint;
begin
  if Length(_VertexArrays) > 0 then Exit;
  SetLength(_VertexArrays, Length(_Skin.Subsets));
  SetLength(_Shaders, Length(_Skin.Subsets));
  SetLength(_Uniforms, Length(_Skin.Subsets));
  glCreateVertexArrays(Length(_VertexArrays), @_VertexArrays[0]);
  for SubsetId := 0 to High(_Skin.Subsets) do
  begin
    SkinSubset := _Skin.Subsets[SubsetId];
    MeshSubset := _Skin.Mesh.Subsets[SubsetId];
    VertexArray := _VertexArrays[SubsetId];
    vd := MeshSubset.VertexDescriptor;
    SkinInfo.BoneCount := Length(_Skin.Joints);
    SkinInfo.BoneWeights := SkinSubset.WeightCount;
    _Shaders[SubsetId] := TShader.AutoShader(vd, _Materials[SubsetId], @SkinInfo);
    _Uniforms[SubsetId].Setup(_Shaders[SubsetId].Ptr);
    glVertexArrayVertexBuffer(
      VertexArray,
      0, MeshSubset.VertexBuffer,
      0, MeshSubset.VertexSize
    );
    AttribOffset := 0;
    for i := 0 to High(vd) do
    begin
      glEnableVertexArrayAttrib(VertexArray, i);
      glVertexArrayAttribFormat(
        VertexArray, i, vd[i].DataCount,
        GL_FLOAT, GL_FALSE, AttribOffset
      );
      glVertexArrayAttribBinding(VertexArray, i, 0);
      AttribOffset += vd[i].Size;
    end;
    glVertexArrayVertexBuffer(
      VertexArray,
      1, SkinSubset.VertexBuffer,
      0, SkinSubset.VertexSize
    );
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
    glVertexArrayElementBuffer(VertexArray, MeshSubset.IndexBuffer);
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
  const Context: TLoadingContext;
  const AParent: TNode;
  const NodeData: TUSceneData.TNodeInterface
);
  var i: Int32;
begin
  _Name := NodeData.Name;
  _Ids := NodeData.Ids;
  Parent := AParent;
  Context.NodeRemap.Add(NodeData, Self);
  _Transform := NodeData.Transform;
  for i := 0 to High(NodeData.Children) do
  begin
    TNode.Create(Context, Self, NodeData.Children[i]);
  end;
end;

destructor TNode.Destroy;
begin
  specialize UArrClear<TNode>(_Children);
  inherited Destroy;
end;

procedure TNode.SetupAttachments(
  const Context: TLoadingContext;
  const NodeData: TUSceneData.TNodeInterface
);
  var i: Int32;
  var AttachMesh: TAttachmentMesh;
  var AttachSkin: TAttachmentSkin;
begin
  for i := 0 to High(NodeData.Attachments) do
  begin
    if NodeData.Attachments[i] is TUSceneData.TAttachmentMesh then
    begin
      AttachMesh := TAttachmentMesh.Create(
        Context,
        TUSceneData.TAttachmentMesh(NodeData.Attachments[i])
      );
      AttachMesh.Node := Self;
    end
    else if NodeData.Attachments[i] is TUSceneData.TAttachmentSkin then
    begin
      AttachSkin := TAttachmentSkin.Create(
        Context,
        TUSceneData.TAttachmentSkin(NodeData.Attachments[i])
      );
      AttachSkin.Node := Self;
    end;
  end;
  for i := 0 to High(NodeData.Children) do
  begin
    Context.NodeRemap.FindValueByKey(
      NodeData.Children[i]
    ).SetupAttachments(
      Context,
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

function TNode.MatchId(const Id: String): Boolean;
  var i: Int32;
  var IdLc: String;
begin
  IdLc := LowerCase(Id);
  for i := 0 to High(_Ids) do
  begin
    if IdLc = _Ids[i] then Exit(True);
  end;
  Result := False;
end;

constructor TLoadingContext.Create(const FilePath: String);
begin
  LoadDir := ExtractFileDir(FilePath);
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

function TForm1.TF_Load(const Args: array of const): TSceneList;
  var Context: TLoadingContext;
  var f, i: Integer;
  var Data: TUSceneDataDAE;
  var FileName: String;
  var Scene: TScene;
  var Timer, TimerData: TUTimer;
begin
  Timer.Start({$I %CURRENTROUTINE%});
  MakeCurrentShared;
  Result := nil;
  for f := 0 to High(Args) do
  begin
    FileName := AnsiString(Args[f].VAnsiString);
    if not FileExists(FileName) then
    begin
      WriteLn('Missing file: ', FileName);
      Continue;
    end;
    Context := TLoadingContext.Create(FileName);
    Data := TUSceneDataDAE.Create([sdo_optimize], sdu_y);
    Scene := TScene.Create;
    try
      Data.Load(FileName);
      TimerData.Start('LoadData ' + FileName);
      SetLength(Scene.Textures, Length(Data.ImageList));
      for i := 0 to High(Scene.Textures) do
      begin
        Scene.Textures[i] := TTexture.Create(Context, Data.ImageList[i]);
        Context.TextureRemap.Add(Data.ImageList[i], Scene.Textures[i]);
      end;
      SetLength(Scene.Meshes, Length(Data.MeshList));
      for i := 0 to High(Scene.Meshes) do
      begin
        Scene.Meshes[i] := TMesh.Create(Data.MeshList[i]);
        Context.MeshRemap.Add(Data.MeshList[i], Scene.Meshes[i]);
      end;
      SetLength(Scene.Skins, Length(Data.SkinList));
      for i := 0 to High(Scene.Skins) do
      begin
        Scene.Skins[i] := TSkin.Create(Context, Data.SkinList[i]);
        Context.SkinRemap.Add(Data.SkinList[i], Scene.Skins[i]);
      end;
      SetLength(Scene.Materials, Length(Data.MaterialList));
      for i := 0 to High(Scene.Materials) do
      begin
        Scene.Materials[i] := TMaterial.Create(Context, Data.MaterialList[i]);
        Context.MaterialRemap.Add(Data.MaterialList[i], Scene.Materials[i]);
      end;
      Scene.RootNode := TNode.Create(Context, nil, Data.RootNode);
      Scene.RootNode.Ptr.SetupAttachments(Context, Data.RootNode);
      Scene.Animation := TAnimation.Create(Data.AnimationList);
      specialize UArrAppend<TSceneShared>(Result, Scene);
      TimerData.Stop;
    finally
      FreeAndNil(Data);
      FreeAndNil(Context);
    end;
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
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('siren/siren_anim.dae')]);
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('Vanguard By T. Choonyung/Vanguard By T. Choonyung.dae')]);
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('Vampire A Lusth/Vampire A Lusth.dae')]);
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('X Bot.dae')]);
  //first file provides the scene, additional files add animations (must be compatible with the scene)
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('box.dae')]);
  //TaskLoad := TaskLoad.StartTask(@TF_Load, [AssetsFile('plane.dae')]);
  //{
  TaskLoad := TaskLoad.StartTask(@TF_Load, [
    //AssetsFile('Vampire A Lusth/Vampire A Lusth.dae'),
    AssetsFile('Vanguard By T. Choonyung/Vanguard By T. Choonyung.dae'),
    //AssetsFile('Castle Guard 02/Castle Guard 02.dae'),
    //AssetsFile('Ch15_nonPBR/Ch15_nonPBR.dae'),
    //AssetsFile('Ch02_nonPBR/Ch02_nonPBR.dae'),
    //AssetsFile('Ch26_nonPBR/Ch26_nonPBR.dae'),
    //AssetsFile('X Bot.dae'),
    //AssetsFile('Y Bot.dae'),
    AssetsFile('Arm Stretching Anim.dae'),
    AssetsFile('Excited Anim.dae'),
    AssetsFile('Rumba Dancing Anim.dae'),
    AssetsFile('Breakdance Uprock Var 1 Anim.dae'),
    AssetsFile('Dancing Anim.dae'),
    AssetsFile('Chicken Dance Anim.dae'),
    AssetsFile('Dancing Twerk Anim.dae'),
    AssetsFile('Hip Hop Dancing Anim.dae'),
    AssetsFile('Hip Hop Dancing 2 Anim.dae'),
    AssetsFile('Hip Hop Dancing 3 Anim.dae'),
    AssetsFile('Hip Hop Dancing 4 Anim.dae'),
    AssetsFile('Shuffling Anim.dae'),
    AssetsFile('Snake Hip Hop Dance Anim.dae')
  ]);
  //}
  Caption := 'PasOpenGL Loading...';
end;

procedure TForm1.Finalize;
begin
end;

procedure TForm1.Tick;
  var W, V, P, WVP: TUMat;
  var ViewPos: TUVec3;
  procedure SetupUniforms(
    const Material: TMaterial;
    const UniformGroup: TUniformGroup
  );
    var CurTexture: Int32;
    procedure SetColor(const UniformId: TGLint; const Color: TUColor);
      var ColorVec: TUVec4;
    begin
      if UniformId = -1 then Exit;
      ColorVec := Color;
      glUniform4fv(UniformId, 1, @ColorVec);
    end;
    procedure SetTexture(const UniformId: TGLint; const Texture: TTextureShared);
    begin
      if UniformId = -1 then Exit;
      if not Texture.IsValid then Exit;
      glBindTextureUnit(CurTexture, Texture.Ptr.Handle);
      glUniform1i(UniformId, CurTexture);
      Inc(CurTexture);
    end;
  begin
    CurTexture := 0;
    glUniform3fv(UniformGroup.ViewPos, 1, @ViewPos);
    glUniformMatrix4fv(UniformGroup.W, 1, GL_TRUE, @W);
    glUniformMatrix4fv(UniformGroup.WVP, 1, GL_TRUE, @WVP);
    SetColor(UniformGroup.MatAmbientColor, Material.AmbientColor);
    SetTexture(UniformGroup.MatAmbientTexture, Material.AmbientTexture);
    SetColor(UniformGroup.MatDiffuseColor, Material.DiffuseColor);
    SetTexture(UniformGroup.MatDiffuseTexture, Material.DiffuseTexture);
    SetColor(UniformGroup.MatEmissiveColor, Material.EmissiveColor);
    SetTexture(UniformGroup.MatEmissiveTexture, Material.EmissiveTexture);
    SetColor(UniformGroup.MatSpecularColor, Material.SpecularColor);
    SetTexture(UniformGroup.MatSpecularTexture, Material.SpecularTexture);
    SetTexture(UniformGroup.MatNormalTexture, Material.NormalTexture);
  end;
  procedure DrawNode(const Node: TNode);
    var Attach: TNode.TAttachment;
    var AttachMesh: TNode.TAttachmentMesh;
    var AttachSkin: TNode.TAttachmentSkin;
    var NewBuffer: TGLuint;
    var NewShader: TShader;
    var Material: TMaterial;
    var UniformGroup: TUniformGroup;
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
        UniformGroup := AttachMesh.Uniforms[i];
        NewShader.Use;
        NewBuffer := AttachMesh.VertexArrays[i];
        glBindVertexArray(NewBuffer);
        SetupUniforms(Material, UniformGroup);
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
        UniformGroup := AttachSkin.Uniforms[i];
        NewShader.Use;
        NewBuffer := AttachSkin.VertexArrays[i];
        glBindVertexArray(NewBuffer);
        SetupUniforms(Material, UniformGroup);
        glUniformMatrix4fv(UniformGroup.Bone, Length(AttachSkin.Pose), GL_TRUE, @AttachSkin.Pose[0]);
        AttachSkin.Skin.Mesh.DrawSubset(i);
      end;
    end;
    for i := 0 to High(Node.Children) do
    begin
      DrawNode(Node.Children[i]);
    end;
  end;
  var t, f, bt: TUFloat;
  var i: Int32;
begin
  W := TUMat.Identity;
  //W := TUMat.RotationX(UHalfPi * 0.5);
  W := TUMat.Scaling(0.05);
  W := W * TUMat.RotationY(((GetTickCount64 mod 6000) / 6000) * UTwoPi);
  ViewPos := [0, 5, 0];
  //V := TUMat.View([10, 10, 10], [0, 5, 0], [0, 1, 0]);
  V := TUMat.View([0, 5, -10], ViewPos, [0, 1, 0]);
  //V := TUMat.View([0, 5, -10], [0, 0, 0], [0, 1, 0]);
  P := TUMat.Proj(UPi * 0.3, ClientWidth / ClientHeight, 0.1, 100);
  WVP := W * V * P;

  glViewport(0, 0, ClientWidth, ClientHeight);
  glEnable(GL_DEPTH_TEST);
  //glClearColor(0.4, 1, 0.8, 1);
  glClearColor(0.2, 0.2, 0.3, 1);
  glClearDepth(1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if TaskLoad.IsComplete then
  begin
    Scenes := TaskLoad.TaskResult;
    TaskLoad.Reset;
    SetupScenes;
    PrintScene;
    Caption := 'PasOpenGL';
  end;

  if not RootNode.IsValid then Exit;
  t := (GetTickCount64 - AppStartTime) * 0.001;
  if AnimBlend.IsValid then
  begin
    bt := t * 0.1;
    if Trunc(bt) <> Trunc(AnimTime) then
    begin
      Anims[0] := Anims[1];
      Anims[1] := Random(AnimBlend.Ptr.BlendableCount);
      if (Anims[0] = Anims[1]) then
      begin
        Anims[1] := (Anims[0] + 1) mod AnimBlend.Ptr.BlendableCount;
      end;
    end;
    AnimTime := bt;
    f := Sin(ULerp(-UHalfPi, UHalfPi, Frac(bt)));// * 0.5 + 0.5;
    f := (Power(Abs(f), 0.5) * USignOf(f)) * 0.5 + 0.5;
    for i := 0 to AnimBlend.Ptr.BlendableCount - 1 do
    begin
      if i = Anims[0] then AnimBlend.Ptr.SetWeight(i, 1 - f)
      else if i = Anims[1] then AnimBlend.Ptr.SetWeight(i, f)
      else AnimBlend.Ptr.SetWeight(i, 0);
    end;
    AnimBlend.Ptr.Apply(t);
  end
  else if AnimInstance.IsValid then
  begin
    AnimInstance.Ptr.Apply(t);
  end;
  DrawNode(RootNode.Ptr);
  glBindVertexArray(0);
end;

procedure TForm1.SetupScenes;
  var i: Int32;
  var RootScene: TScene;
  var Retarget: String;
  var BlendInstances: array of TAnimationInstanceShared;
begin
  for i := 0 to High(Scenes) do
  begin
    Scenes[i].Ptr.RootNode.Ptr.Setup;
    if RootNode.IsValid then Continue;
    RootScene := Scenes[i].Ptr;
    RootNode := RootScene.RootNode;
  end;
  for i := High(Scenes) downto 0 do
  begin
    if not Scenes[i].Ptr.Animation.IsValid then Continue;
    if (RootScene <> Scenes[i].Ptr)
    and RootScene.Animation.IsValid
    and (RootScene.Animation.Ptr.Retarget <> Scenes[i].Ptr.Animation.Ptr.Retarget) then
    begin
      Retarget := RootScene.Animation.Ptr.Retarget;
    end
    else
    begin
      Retarget := '';
    end;
    AnimInstance := TAnimationInstance.Create(
      Scenes[i].Ptr.Animation.Ptr,
      RootNode.Ptr,
      Retarget
    );
    Break;
  end;
  if Length(Scenes) < 3 then Exit;
  BlendInstances := nil;
  for i := 1 to High(Scenes) do
  begin
    if not Scenes[i].Ptr.Animation.IsValid then Continue;
    Retarget := RootScene.Animation.Ptr.Retarget;
    specialize UArrAppend<TAnimationInstanceShared>(
      BlendInstances, TAnimationInstance.Create(
        Scenes[i].Ptr.Animation.Ptr, RootNode.Ptr, Retarget
      )
    );
  end;
  if Length(BlendInstances) < 2 then Exit;
  AnimBlend := TAnimationBlend.Create(BlendInstances);
  for i := 0 to High(BlendInstances) do
  begin
    AnimBlend.Ptr.SetWeight(BlendInstances[i].Ptr, 1);
  end;
  Anims[0] := Random(Length(BlendInstances));
  Anims[1] := (Anims[0] + 1) mod Length(BlendInstances);
  AnimTime := 0;
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

