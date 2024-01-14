// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "SH_Env_Tileable"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_DISTANCE_FIELD_NOT_SUPPORTED("DISTANCE_FIELD_NOT_SUPPORTED", Float) = 1
		_Layer01FlowIntensity("Layer 01 Flow Intensity", Float) = 0
		_DistanceFieldUVScale("Distance Field UV Scale", Float) = 0.32
		_UVWorldXYScale("UV World XY Scale", Float) = 1
		_FlowUVScale("Flow UV Scale", Float) = 1
		_FlowBlend("Flow Blend", Float) = 0
		_FlowStrength("Flow Strength", Float) = 0
		[Toggle(_SWITCHTOWORLDTRIPLANAR_ON)] _SwitchtoWorldTriplanar("Switch to World Triplanar", Float) = 0
		_Blend01Bias("Blend 01 Bias", Float) = 1
		_Blend01Contrast("Blend 01 Contrast", Float) = 1
		_Blend02Bias("Blend 02 Bias", Float) = 1
		_Blend02Contrast("Blend 02 Contrast", Float) = 1
		_Blend03Bias("Blend 03 Bias", Float) = 1
		_Blend03Contrast("Blend 03 Contrast", Float) = 1
		_Layer01NormalFlatten("Layer 01 Normal Flatten", Float) = 0
		_Layer02NormalFlatten("Layer 02 Normal Flatten", Float) = 0
		_Layer03NormalFlatten("Layer 03 Normal Flatten", Float) = 0
		_Layer04NormalFlatten("Layer 04 Normal Flatten", Float) = 0
		_BlendsDivisor("Blends Divisor", Float) = 1
		_BlendsOffset("Blends Offset", Float) = 0.1
		_DistanceBlendContrast("Distance Blend Contrast", Float) = 4
		[Toggle(_BLENDON_ON)] _BlendOn("Blend On", Float) = 0
		[NoScaleOffset][Normal]_Layer01Normal("Layer 01 Normal", 2D) = "white" {}
		[NoScaleOffset]_Layer01RAOHMTexture("Layer 01 RAOHM Texture", 2D) = "white" {}
		[NoScaleOffset][Normal]_Layer02Normal("Layer 02 Normal", 2D) = "bump" {}
		[NoScaleOffset]_Layer02RAOHMTexture("Layer 02 RAOHM Texture", 2D) = "white" {}
		[NoScaleOffset][Normal]_Layer03Normal("Layer 03 Normal", 2D) = "bump" {}
		[NoScaleOffset]_Layer03RAOHMTexture("Layer 03 RAOHM Texture", 2D) = "white" {}
		[NoScaleOffset][Normal]_Layer04Normal("Layer 04 Normal", 2D) = "bump" {}
		[NoScaleOffset]_Layer04RAOHMTexture("Layer 04 RAOHM Texture", 2D) = "white" {}
		[NoScaleOffset][Normal]_TerrainNormal("Terrain Normal", 2D) = "bump" {}
		_Layer01AColor("Layer 01 A Color", Color) = (0,0,0,0)
		_Layer01BColor("Layer 01 B Color", Color) = (0,0,0,0)
		_Layer02AColor("Layer 02 A Color", Color) = (1,0,0,1)
		_Layer02BColor("Layer 02 B Color", Color) = (1,0,0,1)
		_Layer03AColor("Layer 03 A Color", Color) = (0,1,0,1)
		_Layer03BColor("Layer 03 B Color", Color) = (0,1,0,1)
		_Layer04AColor("Layer 04 A Color", Color) = (0,0,1,1)
		_Layer04BColor("Layer 04 B Color", Color) = (0,0,1,1)
		_Layer1Spec("Layer 1 Spec", Float) = 0.25
		_Layer2Spec("Layer 2 Spec", Float) = 0.25
		_Layer3Spec("Layer 3 Spec", Float) = 0.25
		_Layer4Spec("Layer 4 Spec", Float) = 0.25
		_RoughnessMax("Roughness Max", Float) = 0
		_RoughnessMin("Roughness Min", Float) = 0
		_Layer01Tiling("Layer 01 Tiling", Float) = 1
		_Layer02Tiling("Layer 02 Tiling", Float) = 1
		_Layer03Tiling("Layer 03 Tiling", Float) = 1
		[ASEEnd]_Layer04Tiling("Layer 04 Tiling", Float) = 1
		[HideInInspector]_Texture3("Texture 3", 2D) = "white" {}

		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
		//_TransStrength( "Trans Strength", Range( 0, 50 ) ) = 1
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.5
		//_TransScattering( "Trans Scattering", Range( 1, 50 ) ) = 2
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 0.9
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.1
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 0.5
		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		Cull Back
		AlphaToMask Off
		
		HLSLINCLUDE
		#pragma target 2.0

		#pragma prefer_hlslcc gles
		#pragma exclude_renderers d3d11_9x 

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS

		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma multi_compile _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS _ADDITIONAL_OFF
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_TANGENT
			#define ASE_NEEDS_FRAG_WORLD_BITANGENT
			#pragma shader_feature_local _BLENDON_ON
			#pragma shader_feature_local _SWITCHTOWORLDTRIPLANAR_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_color : COLOR;
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Layer01AColor;
			float4 _Layer04BColor;
			float4 _Layer04AColor;
			float4 _Layer03AColor;
			float4 _Layer02BColor;
			float4 _Layer02AColor;
			float4 _Layer03BColor;
			float4 _Layer01BColor;
			float _FlowBlend;
			float _Layer4Spec;
			float _Layer3Spec;
			float _Layer2Spec;
			float _Layer1Spec;
			float _Layer04NormalFlatten;
			float _Layer03NormalFlatten;
			float _Layer02NormalFlatten;
			float _Layer01NormalFlatten;
			float _DistanceBlendContrast;
			float _Blend03Bias;
			float _Blend03Contrast;
			float _Layer04Tiling;
			float _FlowStrength;
			float _UVWorldXYScale;
			float _Blend02Contrast;
			float _Layer03Tiling;
			float _RoughnessMin;
			float _DISTANCE_FIELD_NOT_SUPPORTED;
			float _Blend01Bias;
			float _BlendsDivisor;
			float _BlendsOffset;
			float _Blend01Contrast;
			float _Layer02Tiling;
			float _DistanceFieldUVScale;
			float _FlowUVScale;
			float _Layer01Tiling;
			float _Layer01FlowIntensity;
			float _Blend02Bias;
			float _RoughnessMax;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _Layer01RAOHMTexture;
			sampler2D _Texture3;
			sampler2D _Layer02RAOHMTexture;
			sampler2D _Layer03RAOHMTexture;
			sampler2D _Layer04RAOHMTexture;
			sampler2D _Layer01Normal;
			sampler2D _Layer02Normal;
			sampler2D _Layer03Normal;
			sampler2D _Layer04Normal;
			sampler2D _TerrainNormal;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_color = v.ase_color;
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag ( VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float2 temp_output_131_0 = (( WorldPosition / ( _UVWorldXYScale * 512.0 ) )).xy;
				float2 temp_output_21_0_g83 = temp_output_131_0;
				float temp_output_11_0_g83 = 0.0;
				float3 temp_cast_0 = (( _DISTANCE_FIELD_NOT_SUPPORTED / _DistanceFieldUVScale )).xxx;
				float3 worldToObjDir143 = mul( GetWorldToObjectMatrix(), float4( temp_cast_0, 0 ) ).xyz;
				float2 normalizeResult138 = normalize( ( ( (worldToObjDir143).xy + ( (tex2D( _Texture3, (( WorldPosition / ( _FlowUVScale * 512.0 ) )).xy )).rg / float2( 32,0 ) ) ) - float2( 0.5,0 ) ) );
				float2 lerpResult134 = lerp( temp_output_131_0 , ( normalizeResult138 * _FlowStrength ) , _FlowBlend);
				float2 temp_output_15_0_g83 = ( lerpResult134 * float2( -1,1 ) );
				float2 temp_output_16_0_g83 = ( frac( ( temp_output_11_0_g83 - 0.5 ) ) * temp_output_15_0_g83 );
				float2 temp_output_19_0_g83 = ( temp_output_21_0_g83 + temp_output_16_0_g83 );
				float temp_output_12_0_g83 = frac( temp_output_11_0_g83 );
				float2 temp_output_13_0_g83 = ( temp_output_15_0_g83 * temp_output_12_0_g83 );
				float2 temp_output_22_0_g83 = ( temp_output_13_0_g83 + temp_output_21_0_g83 );
				float temp_output_2_0_g85 = temp_output_12_0_g83;
				float temp_output_3_0_g85 = 1.0;
				float temp_output_4_0_g85 = ( temp_output_2_0_g85 + ( 0.25 * temp_output_3_0_g85 ) );
				float ifLocalVar24_g85 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g85 = temp_output_2_0_g85;
				else
				ifLocalVar24_g85 = temp_output_4_0_g85;
				float temp_output_7_0_g85 = frac( ( ifLocalVar24_g85 / temp_output_3_0_g85 ) );
				float temp_output_8_0_g85 = ( 2.0 * temp_output_7_0_g85 );
				float temp_output_12_0_g85 = floor( temp_output_8_0_g85 );
				float lerpResult13_g85 = lerp( temp_output_8_0_g85 , ( 2.0 * ( 1.0 - temp_output_7_0_g85 ) ) , temp_output_12_0_g85);
				float temp_output_78_0_g83 = lerpResult13_g85;
				float2 lerpResult60_g83 = lerp( temp_output_19_0_g83 , temp_output_22_0_g83 , temp_output_78_0_g83);
				float2 temp_output_124_62 = lerpResult60_g83;
				float2 lerpResult114 = lerp( temp_output_131_0 , temp_output_124_62 , _Layer01FlowIntensity);
				float2 temp_output_105_0 = ( lerpResult114 * _Layer01Tiling );
				float temp_output_23_0_g94 = 0.0;
				float3 temp_cast_1 = (( _Layer01Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g94 = ( WorldPosition / ( abs( temp_cast_1 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xz );
				float4 ifLocalVar24_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar24_g94 = float4( (tex2DNode31_g94).rgb , 0.0 );
				else
				ifLocalVar24_g94 = tex2DNode31_g94;
				float4 tex2DNode29_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).yz );
				float4 ifLocalVar21_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar21_g94 = float4( (tex2DNode29_g94).rgb , 0.0 );
				else
				ifLocalVar21_g94 = tex2DNode29_g94;
				float temp_output_9_0_g94 = 0.5;
				float temp_output_1_0_g95 = temp_output_9_0_g94;
				float3 normalizedWorldNormal = normalize( WorldNormal );
				float3 temp_output_15_0_g94 = normalizedWorldNormal;
				float lerpResult5_g95 = lerp( ( 0.0 - temp_output_1_0_g95 ) , ( temp_output_1_0_g95 + 1.0 ) , abs( (temp_output_15_0_g94).x ));
				float4 lerpResult3_g94 = lerp( ifLocalVar24_g94 , ifLocalVar21_g94 , saturate( lerpResult5_g95 ));
				float4 tex2DNode32_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xy );
				float4 ifLocalVar25_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar25_g94 = float4( (tex2DNode32_g94).rgb , 0.0 );
				else
				ifLocalVar25_g94 = tex2DNode32_g94;
				float temp_output_1_0_g96 = temp_output_9_0_g94;
				float lerpResult5_g96 = lerp( ( 0.0 - temp_output_1_0_g96 ) , ( temp_output_1_0_g96 + 1.0 ) , abs( (temp_output_15_0_g94).z ));
				float4 lerpResult4_g94 = lerp( lerpResult3_g94 , ifLocalVar25_g94 , saturate( lerpResult5_g96 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch82 = lerpResult4_g94;
				#else
				float4 staticSwitch82 = tex2D( _Layer01RAOHMTexture, temp_output_105_0 );
				#endif
				float temp_output_74_0 = ( 1.0 - (staticSwitch82).b );
				float4 lerpResult59 = lerp( _Layer01AColor , _Layer01BColor , temp_output_74_0);
				float2 lerpResult115 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_106_0 = ( lerpResult115 * _Layer02Tiling );
				float temp_output_23_0_g91 = 0.0;
				float3 temp_cast_5 = (( _Layer02Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g91 = ( WorldPosition / ( abs( temp_cast_5 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xz );
				float4 ifLocalVar24_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar24_g91 = float4( (tex2DNode31_g91).rgb , 0.0 );
				else
				ifLocalVar24_g91 = tex2DNode31_g91;
				float4 tex2DNode29_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).yz );
				float4 ifLocalVar21_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar21_g91 = float4( (tex2DNode29_g91).rgb , 0.0 );
				else
				ifLocalVar21_g91 = tex2DNode29_g91;
				float temp_output_9_0_g91 = 0.5;
				float temp_output_1_0_g92 = temp_output_9_0_g91;
				float3 temp_output_15_0_g91 = normalizedWorldNormal;
				float lerpResult5_g92 = lerp( ( 0.0 - temp_output_1_0_g92 ) , ( temp_output_1_0_g92 + 1.0 ) , abs( (temp_output_15_0_g91).x ));
				float4 lerpResult3_g91 = lerp( ifLocalVar24_g91 , ifLocalVar21_g91 , saturate( lerpResult5_g92 ));
				float4 tex2DNode32_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xy );
				float4 ifLocalVar25_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar25_g91 = float4( (tex2DNode32_g91).rgb , 0.0 );
				else
				ifLocalVar25_g91 = tex2DNode32_g91;
				float temp_output_1_0_g93 = temp_output_9_0_g91;
				float lerpResult5_g93 = lerp( ( 0.0 - temp_output_1_0_g93 ) , ( temp_output_1_0_g93 + 1.0 ) , abs( (temp_output_15_0_g91).z ));
				float4 lerpResult4_g91 = lerp( lerpResult3_g91 , ifLocalVar25_g91 , saturate( lerpResult5_g93 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch83 = lerpResult4_g91;
				#else
				float4 staticSwitch83 = tex2D( _Layer02RAOHMTexture, temp_output_106_0 );
				#endif
				float temp_output_77_0 = ( 1.0 - (staticSwitch83).b );
				float4 lerpResult60 = lerp( _Layer02AColor , _Layer02BColor , temp_output_77_0);
				float temp_output_1_0_g246 = _Blend01Contrast;
				float temp_output_2_0_g245 = temp_output_74_0;
				float4 break196 = ( ( IN.ase_color + _BlendsOffset ) / _BlendsDivisor );
				float temp_output_1_0_g245 = pow( ( 1.0 - saturate( break196.r ) ) , _Blend01Bias );
				float temp_output_3_0_g245 = temp_output_77_0;
				float temp_output_17_0_g245 = saturate( ( ( ( temp_output_2_0_g245 + ( 1.0 - temp_output_1_0_g245 ) ) + 1.0 ) - ( ( temp_output_1_0_g245 * 2.0 ) + temp_output_3_0_g245 ) ) );
				float lerpResult5_g246 = lerp( ( 0.0 - temp_output_1_0_g246 ) , ( temp_output_1_0_g246 + 1.0 ) , temp_output_17_0_g245);
				float temp_output_24_0_g245 = saturate( lerpResult5_g246 );
				float temp_output_50_0 = temp_output_24_0_g245;
				float4 lerpResult56 = lerp( lerpResult59 , lerpResult60 , temp_output_50_0);
				float2 lerpResult116 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_107_0 = ( lerpResult116 * _Layer03Tiling );
				float temp_output_23_0_g97 = 0.0;
				float3 temp_cast_9 = (( _Layer03Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g97 = ( WorldPosition / ( abs( temp_cast_9 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xz );
				float4 ifLocalVar24_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar24_g97 = float4( (tex2DNode31_g97).rgb , 0.0 );
				else
				ifLocalVar24_g97 = tex2DNode31_g97;
				float4 tex2DNode29_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).yz );
				float4 ifLocalVar21_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar21_g97 = float4( (tex2DNode29_g97).rgb , 0.0 );
				else
				ifLocalVar21_g97 = tex2DNode29_g97;
				float temp_output_9_0_g97 = 0.5;
				float temp_output_1_0_g98 = temp_output_9_0_g97;
				float3 temp_output_15_0_g97 = normalizedWorldNormal;
				float lerpResult5_g98 = lerp( ( 0.0 - temp_output_1_0_g98 ) , ( temp_output_1_0_g98 + 1.0 ) , abs( (temp_output_15_0_g97).x ));
				float4 lerpResult3_g97 = lerp( ifLocalVar24_g97 , ifLocalVar21_g97 , saturate( lerpResult5_g98 ));
				float4 tex2DNode32_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xy );
				float4 ifLocalVar25_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar25_g97 = float4( (tex2DNode32_g97).rgb , 0.0 );
				else
				ifLocalVar25_g97 = tex2DNode32_g97;
				float temp_output_1_0_g99 = temp_output_9_0_g97;
				float lerpResult5_g99 = lerp( ( 0.0 - temp_output_1_0_g99 ) , ( temp_output_1_0_g99 + 1.0 ) , abs( (temp_output_15_0_g97).z ));
				float4 lerpResult4_g97 = lerp( lerpResult3_g97 , ifLocalVar25_g97 , saturate( lerpResult5_g99 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch84 = lerpResult4_g97;
				#else
				float4 staticSwitch84 = tex2D( _Layer03RAOHMTexture, temp_output_107_0 );
				#endif
				float temp_output_79_0 = ( 1.0 - (staticSwitch84).b );
				float4 lerpResult61 = lerp( _Layer03AColor , _Layer03BColor , temp_output_79_0);
				float temp_output_1_0_g290 = _Blend02Contrast;
				float lerpResult16_g245 = lerp( temp_output_3_0_g245 , temp_output_2_0_g245 , temp_output_24_0_g245);
				float temp_output_2_0_g289 = lerpResult16_g245;
				float temp_output_1_0_g289 = pow( ( 1.0 - saturate( break196.g ) ) , _Blend02Bias );
				float temp_output_3_0_g289 = temp_output_79_0;
				float temp_output_17_0_g289 = saturate( ( ( ( temp_output_2_0_g289 + ( 1.0 - temp_output_1_0_g289 ) ) + 1.0 ) - ( ( temp_output_1_0_g289 * 2.0 ) + temp_output_3_0_g289 ) ) );
				float lerpResult5_g290 = lerp( ( 0.0 - temp_output_1_0_g290 ) , ( temp_output_1_0_g290 + 1.0 ) , temp_output_17_0_g289);
				float temp_output_24_0_g289 = saturate( lerpResult5_g290 );
				float temp_output_51_0 = temp_output_24_0_g289;
				float4 lerpResult57 = lerp( lerpResult56 , lerpResult61 , temp_output_51_0);
				float2 lerpResult117 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_108_0 = ( lerpResult117 * _Layer04Tiling );
				float temp_output_23_0_g100 = 0.0;
				float3 temp_cast_13 = (( _Layer04Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g100 = ( WorldPosition / ( abs( temp_cast_13 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xz );
				float4 ifLocalVar24_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar24_g100 = float4( (tex2DNode31_g100).rgb , 0.0 );
				else
				ifLocalVar24_g100 = tex2DNode31_g100;
				float4 tex2DNode29_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).yz );
				float4 ifLocalVar21_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar21_g100 = float4( (tex2DNode29_g100).rgb , 0.0 );
				else
				ifLocalVar21_g100 = tex2DNode29_g100;
				float temp_output_9_0_g100 = 0.5;
				float temp_output_1_0_g101 = temp_output_9_0_g100;
				float3 temp_output_15_0_g100 = normalizedWorldNormal;
				float lerpResult5_g101 = lerp( ( 0.0 - temp_output_1_0_g101 ) , ( temp_output_1_0_g101 + 1.0 ) , abs( (temp_output_15_0_g100).x ));
				float4 lerpResult3_g100 = lerp( ifLocalVar24_g100 , ifLocalVar21_g100 , saturate( lerpResult5_g101 ));
				float4 tex2DNode32_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xy );
				float4 ifLocalVar25_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar25_g100 = float4( (tex2DNode32_g100).rgb , 0.0 );
				else
				ifLocalVar25_g100 = tex2DNode32_g100;
				float temp_output_1_0_g102 = temp_output_9_0_g100;
				float lerpResult5_g102 = lerp( ( 0.0 - temp_output_1_0_g102 ) , ( temp_output_1_0_g102 + 1.0 ) , abs( (temp_output_15_0_g100).z ));
				float4 lerpResult4_g100 = lerp( lerpResult3_g100 , ifLocalVar25_g100 , saturate( lerpResult5_g102 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch85 = lerpResult4_g100;
				#else
				float4 staticSwitch85 = tex2D( _Layer04RAOHMTexture, temp_output_108_0 );
				#endif
				float temp_output_81_0 = ( 1.0 - (staticSwitch85).b );
				float4 lerpResult62 = lerp( _Layer04AColor , _Layer04BColor , temp_output_81_0);
				float temp_output_1_0_g292 = _Blend03Contrast;
				float lerpResult16_g289 = lerp( temp_output_3_0_g289 , temp_output_2_0_g289 , temp_output_24_0_g289);
				float temp_output_2_0_g291 = lerpResult16_g289;
				float temp_output_1_0_g291 = pow( ( 1.0 - saturate( break196.b ) ) , _Blend03Bias );
				float temp_output_3_0_g291 = temp_output_81_0;
				float temp_output_17_0_g291 = saturate( ( ( ( temp_output_2_0_g291 + ( 1.0 - temp_output_1_0_g291 ) ) + 1.0 ) - ( ( temp_output_1_0_g291 * 2.0 ) + temp_output_3_0_g291 ) ) );
				float lerpResult5_g292 = lerp( ( 0.0 - temp_output_1_0_g292 ) , ( temp_output_1_0_g292 + 1.0 ) , temp_output_17_0_g291);
				float temp_output_24_0_g291 = saturate( lerpResult5_g292 );
				float temp_output_52_0 = temp_output_24_0_g291;
				float4 lerpResult58 = lerp( lerpResult57 , lerpResult62 , temp_output_52_0);
				float temp_output_1_0_g311 = _DistanceBlendContrast;
				float lerpResult5_g311 = lerp( ( 0.0 - temp_output_1_0_g311 ) , ( temp_output_1_0_g311 + 1.0 ) , ( ( 20.0 + 0.0 ) / 2.0 ));
				float temp_output_26_0 = saturate( lerpResult5_g311 );
				float4 lerpResult17 = lerp( lerpResult58 , lerpResult62 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float4 staticSwitch15 = lerpResult17;
				#else
				float4 staticSwitch15 = lerpResult58;
				#endif
				
				int temp_output_76_0_g247 = 0;
				int temp_output_75_0_g247 = (int)1.0;
				float3 temp_output_72_0_g247 = WorldPosition;
				float temp_output_69_0_g247 = ( abs( ( _Layer01Tiling * 5.12 ) ) * -1.0 );
				float3 temp_output_68_0_g247 = ( temp_output_72_0_g247 / temp_output_69_0_g247 );
				float4 tex2DNode27_g247 = tex2D( _Layer01Normal, (temp_output_68_0_g247).xz );
				float3x3 ase_tangentToWorldFast = float3x3(WorldTangent.x,WorldBiTangent.x,WorldNormal.x,WorldTangent.y,WorldBiTangent.y,WorldNormal.y,WorldTangent.z,WorldBiTangent.z,WorldNormal.z);
				float3 tangentToWorldDir63_g247 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g247 = ( tangentToWorldDir63_g247 * float3( 1,0,0 ) );
				float3 appendResult35_g247 = (float3(( tex2DNode27_g247.r * -1.0 ) , ( tex2DNode27_g247.b * (temp_output_54_0_g247).y ) , ( tex2DNode27_g247.g * -1.0 )));
				float4 tex2DNode28_g247 = tex2D( _Layer01Normal, (temp_output_68_0_g247).yz );
				float temp_output_51_0_g247 = (temp_output_54_0_g247).x;
				float3 appendResult36_g247 = (float3(( tex2DNode28_g247.r * -1.0 ) , ( tex2DNode28_g247.b * temp_output_51_0_g247 ) , ( tex2DNode28_g247.g * -1.0 )));
				float temp_output_1_0_g248 = 0.0;
				float lerpResult5_g248 = lerp( ( 0.0 - temp_output_1_0_g248 ) , ( temp_output_1_0_g248 + 1.0 ) , abs( temp_output_51_0_g247 ));
				float temp_output_58_0_g247 = saturate( lerpResult5_g248 );
				float3 lerpResult24_g247 = lerp( appendResult35_g247 , appendResult36_g247 , temp_output_58_0_g247);
				float4 tex2DNode29_g247 = tex2D( _Layer01Normal, (temp_output_68_0_g247).xy );
				float temp_output_53_0_g247 = (temp_output_54_0_g247).z;
				float3 appendResult50_g247 = (float3(( (tex2DNode29_g247).rg * float2( -1,-1 ) ) , ( tex2DNode29_g247.b * temp_output_53_0_g247 )));
				float temp_output_1_0_g260 = 0.5;
				float lerpResult5_g260 = lerp( ( 0.0 - temp_output_1_0_g260 ) , ( temp_output_1_0_g260 + 1.0 ) , abs( temp_output_53_0_g247 ));
				float temp_output_59_0_g247 = saturate( lerpResult5_g260 );
				float3 lerpResult25_g247 = lerp( lerpResult24_g247 , appendResult50_g247 , temp_output_59_0_g247);
				float3 normalizeResult20_g247 = normalize( lerpResult25_g247 );
				float3 temp_output_22_0_g249 = WorldNormal;
				float3 normalizeResult6_g252 = normalize( temp_output_22_0_g249 );
				float3 normalizeResult7_g252 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g252 = cross( normalizeResult6_g252 , normalizeResult7_g252 );
				float3 temp_output_10_0_g253 = temp_output_5_0_g252;
				float dotResult5_g253 = dot( temp_output_10_0_g253 , temp_output_10_0_g253 );
				float3 normalizeResult7_g253 = normalize( temp_output_10_0_g253 );
				float2 appendResult8_g253 = (float2(normalizeResult7_g253.x , 0.0));
				float2 appendResult11_g253 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g253 = 0;
				if( dotResult5_g253 <= 1E-06 )
				ifLocalVar4_g253 = appendResult11_g253;
				else
				ifLocalVar4_g253 = appendResult8_g253;
				float3 temp_cast_24 = (temp_output_69_0_g247).xxx;
				float3 temp_output_59_0_g249 = ( temp_output_72_0_g247 / temp_cast_24 );
				float dotResult49_g249 = dot( temp_output_22_0_g249 , float3(0,1,0) );
				float ifLocalVar45_g249 = 0;
				if( dotResult49_g249 > 0.0 )
				ifLocalVar45_g249 = -1.0;
				else if( dotResult49_g249 < 0.0 )
				ifLocalVar45_g249 = 1.0;
				float3 appendResult38_g249 = (float3(ifLocalVar45_g249 , -1.0 , 1.0));
				float dotResult54_g249 = dot( temp_output_22_0_g249 , float3(1,0,0) );
				float ifLocalVar50_g249 = 0;
				if( dotResult54_g249 > 0.0 )
				ifLocalVar50_g249 = 1.0;
				else if( dotResult54_g249 < 0.0 )
				ifLocalVar50_g249 = -1.0;
				float3 appendResult37_g249 = (float3(ifLocalVar50_g249 , -1.0 , 1.0));
				float4 lerpResult27_g249 = lerp( ( tex2D( _Layer01Normal, (temp_output_59_0_g249).xz ) * float4( appendResult38_g249 , 0.0 ) ) , ( tex2D( _Layer01Normal, (temp_output_59_0_g249).yz ) * float4( appendResult37_g249 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g247 ));
				float3 break5_g256 = lerpResult27_g249.rgb;
				float3 temp_output_10_0_g254 = cross( temp_output_5_0_g252 , normalizeResult6_g252 );
				float dotResult5_g254 = dot( temp_output_10_0_g254 , temp_output_10_0_g254 );
				float3 normalizeResult7_g254 = normalize( temp_output_10_0_g254 );
				float2 appendResult8_g254 = (float2(normalizeResult7_g254.x , 0.0));
				float2 appendResult11_g254 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g254 = 0;
				if( dotResult5_g254 <= 1E-06 )
				ifLocalVar4_g254 = appendResult11_g254;
				else
				ifLocalVar4_g254 = appendResult8_g254;
				float3 temp_output_25_0_g249 = ( ( float3( (ifLocalVar4_g253).xy ,  0.0 ) * break5_g256.x ) + ( float3( (ifLocalVar4_g254).xy ,  0.0 ) * break5_g256.y ) + ( normalizeResult6_g252 * break5_g256.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g257 = normalize( temp_output_22_0_g249 );
				float3 normalizeResult7_g257 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g257 = cross( normalizeResult6_g257 , normalizeResult7_g257 );
				float3 temp_output_10_0_g258 = temp_output_5_0_g257;
				float dotResult5_g258 = dot( temp_output_10_0_g258 , temp_output_10_0_g258 );
				float3 normalizeResult7_g258 = normalize( temp_output_10_0_g258 );
				float2 appendResult8_g258 = (float2(normalizeResult7_g258.x , 0.0));
				float2 appendResult11_g258 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g258 = 0;
				if( dotResult5_g258 <= 1E-06 )
				ifLocalVar4_g258 = appendResult11_g258;
				else
				ifLocalVar4_g258 = appendResult8_g258;
				float dotResult20_g249 = dot( temp_output_22_0_g249 , float3(0,0,1) );
				float ifLocalVar16_g249 = 0;
				if( dotResult20_g249 > 0.0 )
				ifLocalVar16_g249 = 1.0;
				else if( dotResult20_g249 < 0.0 )
				ifLocalVar16_g249 = -1.0;
				float3 appendResult14_g249 = (float3(ifLocalVar16_g249 , -1.0 , 1.0));
				float3 break5_g250 = ( tex2D( _Layer01Normal, (temp_output_59_0_g249).xy ) * float4( appendResult14_g249 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g259 = cross( temp_output_5_0_g257 , normalizeResult6_g257 );
				float dotResult5_g259 = dot( temp_output_10_0_g259 , temp_output_10_0_g259 );
				float3 normalizeResult7_g259 = normalize( temp_output_10_0_g259 );
				float2 appendResult8_g259 = (float2(normalizeResult7_g259.x , 0.0));
				float2 appendResult11_g259 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g259 = 0;
				if( dotResult5_g259 <= 1E-06 )
				ifLocalVar4_g259 = appendResult11_g259;
				else
				ifLocalVar4_g259 = appendResult8_g259;
				float3 temp_output_9_0_g249 = ( ( float3( (ifLocalVar4_g258).xy ,  0.0 ) * break5_g250.x ) + ( float3( (ifLocalVar4_g259).xy ,  0.0 ) * break5_g250.y ) + ( normalizeResult6_g257 * break5_g250.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g249 = ( 1.0 - temp_output_59_0_g247 );
				float3 lerpResult5_g249 = lerp( temp_output_25_0_g249 , temp_output_9_0_g249 , temp_output_33_0_g249);
				float3 temp_output_18_0_g247 = ( (float)temp_output_75_0_g247 == 0.0 ? normalizeResult20_g247 : lerpResult5_g249 );
				float3x3 ase_worldToTangent = float3x3(WorldTangent,WorldBiTangent,WorldNormal);
				float3 worldToTangentDir11_g247 = mul( ase_worldToTangent, temp_output_18_0_g247);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch91 = ( (float)temp_output_76_0_g247 == 0.0 ? worldToTangentDir11_g247 : temp_output_18_0_g247 );
				#else
				float3 staticSwitch91 = UnpackNormalScale( tex2D( _Layer01Normal, temp_output_105_0 ), 1.0f );
				#endif
				float3 lerpResult1_g316 = lerp( staticSwitch91 , float3(0,0,1) , _Layer01NormalFlatten);
				int temp_output_76_0_g261 = 0;
				int temp_output_75_0_g261 = (int)1.0;
				float3 temp_output_72_0_g261 = WorldPosition;
				float temp_output_69_0_g261 = ( abs( ( _Layer02Tiling * 5.12 ) ) * -1.0 );
				float3 temp_output_68_0_g261 = ( temp_output_72_0_g261 / temp_output_69_0_g261 );
				float4 tex2DNode27_g261 = tex2D( _Layer02Normal, (temp_output_68_0_g261).xz );
				float3 tangentToWorldDir63_g261 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g261 = ( tangentToWorldDir63_g261 * float3( 1,0,0 ) );
				float3 appendResult35_g261 = (float3(( tex2DNode27_g261.r * -1.0 ) , ( tex2DNode27_g261.b * (temp_output_54_0_g261).y ) , ( tex2DNode27_g261.g * -1.0 )));
				float4 tex2DNode28_g261 = tex2D( _Layer02Normal, (temp_output_68_0_g261).yz );
				float temp_output_51_0_g261 = (temp_output_54_0_g261).x;
				float3 appendResult36_g261 = (float3(( tex2DNode28_g261.r * -1.0 ) , ( tex2DNode28_g261.b * temp_output_51_0_g261 ) , ( tex2DNode28_g261.g * -1.0 )));
				float temp_output_1_0_g262 = 0.0;
				float lerpResult5_g262 = lerp( ( 0.0 - temp_output_1_0_g262 ) , ( temp_output_1_0_g262 + 1.0 ) , abs( temp_output_51_0_g261 ));
				float temp_output_58_0_g261 = saturate( lerpResult5_g262 );
				float3 lerpResult24_g261 = lerp( appendResult35_g261 , appendResult36_g261 , temp_output_58_0_g261);
				float4 tex2DNode29_g261 = tex2D( _Layer02Normal, (temp_output_68_0_g261).xy );
				float temp_output_53_0_g261 = (temp_output_54_0_g261).z;
				float3 appendResult50_g261 = (float3(( (tex2DNode29_g261).rg * float2( -1,-1 ) ) , ( tex2DNode29_g261.b * temp_output_53_0_g261 )));
				float temp_output_1_0_g274 = 0.5;
				float lerpResult5_g274 = lerp( ( 0.0 - temp_output_1_0_g274 ) , ( temp_output_1_0_g274 + 1.0 ) , abs( temp_output_53_0_g261 ));
				float temp_output_59_0_g261 = saturate( lerpResult5_g274 );
				float3 lerpResult25_g261 = lerp( lerpResult24_g261 , appendResult50_g261 , temp_output_59_0_g261);
				float3 normalizeResult20_g261 = normalize( lerpResult25_g261 );
				float3 temp_output_22_0_g263 = WorldNormal;
				float3 normalizeResult6_g266 = normalize( temp_output_22_0_g263 );
				float3 normalizeResult7_g266 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g266 = cross( normalizeResult6_g266 , normalizeResult7_g266 );
				float3 temp_output_10_0_g267 = temp_output_5_0_g266;
				float dotResult5_g267 = dot( temp_output_10_0_g267 , temp_output_10_0_g267 );
				float3 normalizeResult7_g267 = normalize( temp_output_10_0_g267 );
				float2 appendResult8_g267 = (float2(normalizeResult7_g267.x , 0.0));
				float2 appendResult11_g267 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g267 = 0;
				if( dotResult5_g267 <= 1E-06 )
				ifLocalVar4_g267 = appendResult11_g267;
				else
				ifLocalVar4_g267 = appendResult8_g267;
				float3 temp_cast_46 = (temp_output_69_0_g261).xxx;
				float3 temp_output_59_0_g263 = ( temp_output_72_0_g261 / temp_cast_46 );
				float dotResult49_g263 = dot( temp_output_22_0_g263 , float3(0,1,0) );
				float ifLocalVar45_g263 = 0;
				if( dotResult49_g263 > 0.0 )
				ifLocalVar45_g263 = -1.0;
				else if( dotResult49_g263 < 0.0 )
				ifLocalVar45_g263 = 1.0;
				float3 appendResult38_g263 = (float3(ifLocalVar45_g263 , -1.0 , 1.0));
				float dotResult54_g263 = dot( temp_output_22_0_g263 , float3(1,0,0) );
				float ifLocalVar50_g263 = 0;
				if( dotResult54_g263 > 0.0 )
				ifLocalVar50_g263 = 1.0;
				else if( dotResult54_g263 < 0.0 )
				ifLocalVar50_g263 = -1.0;
				float3 appendResult37_g263 = (float3(ifLocalVar50_g263 , -1.0 , 1.0));
				float4 lerpResult27_g263 = lerp( ( tex2D( _Layer02Normal, (temp_output_59_0_g263).xz ) * float4( appendResult38_g263 , 0.0 ) ) , ( tex2D( _Layer02Normal, (temp_output_59_0_g263).yz ) * float4( appendResult37_g263 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g261 ));
				float3 break5_g270 = lerpResult27_g263.rgb;
				float3 temp_output_10_0_g268 = cross( temp_output_5_0_g266 , normalizeResult6_g266 );
				float dotResult5_g268 = dot( temp_output_10_0_g268 , temp_output_10_0_g268 );
				float3 normalizeResult7_g268 = normalize( temp_output_10_0_g268 );
				float2 appendResult8_g268 = (float2(normalizeResult7_g268.x , 0.0));
				float2 appendResult11_g268 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g268 = 0;
				if( dotResult5_g268 <= 1E-06 )
				ifLocalVar4_g268 = appendResult11_g268;
				else
				ifLocalVar4_g268 = appendResult8_g268;
				float3 temp_output_25_0_g263 = ( ( float3( (ifLocalVar4_g267).xy ,  0.0 ) * break5_g270.x ) + ( float3( (ifLocalVar4_g268).xy ,  0.0 ) * break5_g270.y ) + ( normalizeResult6_g266 * break5_g270.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g271 = normalize( temp_output_22_0_g263 );
				float3 normalizeResult7_g271 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g271 = cross( normalizeResult6_g271 , normalizeResult7_g271 );
				float3 temp_output_10_0_g272 = temp_output_5_0_g271;
				float dotResult5_g272 = dot( temp_output_10_0_g272 , temp_output_10_0_g272 );
				float3 normalizeResult7_g272 = normalize( temp_output_10_0_g272 );
				float2 appendResult8_g272 = (float2(normalizeResult7_g272.x , 0.0));
				float2 appendResult11_g272 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g272 = 0;
				if( dotResult5_g272 <= 1E-06 )
				ifLocalVar4_g272 = appendResult11_g272;
				else
				ifLocalVar4_g272 = appendResult8_g272;
				float dotResult20_g263 = dot( temp_output_22_0_g263 , float3(0,0,1) );
				float ifLocalVar16_g263 = 0;
				if( dotResult20_g263 > 0.0 )
				ifLocalVar16_g263 = 1.0;
				else if( dotResult20_g263 < 0.0 )
				ifLocalVar16_g263 = -1.0;
				float3 appendResult14_g263 = (float3(ifLocalVar16_g263 , -1.0 , 1.0));
				float3 break5_g264 = ( tex2D( _Layer02Normal, (temp_output_59_0_g263).xy ) * float4( appendResult14_g263 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g273 = cross( temp_output_5_0_g271 , normalizeResult6_g271 );
				float dotResult5_g273 = dot( temp_output_10_0_g273 , temp_output_10_0_g273 );
				float3 normalizeResult7_g273 = normalize( temp_output_10_0_g273 );
				float2 appendResult8_g273 = (float2(normalizeResult7_g273.x , 0.0));
				float2 appendResult11_g273 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g273 = 0;
				if( dotResult5_g273 <= 1E-06 )
				ifLocalVar4_g273 = appendResult11_g273;
				else
				ifLocalVar4_g273 = appendResult8_g273;
				float3 temp_output_9_0_g263 = ( ( float3( (ifLocalVar4_g272).xy ,  0.0 ) * break5_g264.x ) + ( float3( (ifLocalVar4_g273).xy ,  0.0 ) * break5_g264.y ) + ( normalizeResult6_g271 * break5_g264.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g263 = ( 1.0 - temp_output_59_0_g261 );
				float3 lerpResult5_g263 = lerp( temp_output_25_0_g263 , temp_output_9_0_g263 , temp_output_33_0_g263);
				float3 temp_output_18_0_g261 = ( (float)temp_output_75_0_g261 == 0.0 ? normalizeResult20_g261 : lerpResult5_g263 );
				float3 worldToTangentDir11_g261 = mul( ase_worldToTangent, temp_output_18_0_g261);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch88 = ( (float)temp_output_76_0_g261 == 0.0 ? worldToTangentDir11_g261 : temp_output_18_0_g261 );
				#else
				float3 staticSwitch88 = UnpackNormalScale( tex2D( _Layer02Normal, temp_output_106_0 ), 1.0f );
				#endif
				float3 lerpResult1_g317 = lerp( staticSwitch88 , float3(0,0,1) , _Layer02NormalFlatten);
				float3 lerpResult46 = lerp( lerpResult1_g316 , lerpResult1_g317 , temp_output_50_0);
				int temp_output_76_0_g275 = 0;
				int temp_output_75_0_g275 = (int)1.0;
				float3 temp_output_72_0_g275 = WorldPosition;
				float temp_output_69_0_g275 = ( abs( ( _Layer03Tiling * 5.12 ) ) * -1.0 );
				float3 temp_output_68_0_g275 = ( temp_output_72_0_g275 / temp_output_69_0_g275 );
				float4 tex2DNode27_g275 = tex2D( _Layer03Normal, (temp_output_68_0_g275).xz );
				float3 tangentToWorldDir63_g275 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g275 = ( tangentToWorldDir63_g275 * float3( 1,0,0 ) );
				float3 appendResult35_g275 = (float3(( tex2DNode27_g275.r * -1.0 ) , ( tex2DNode27_g275.b * (temp_output_54_0_g275).y ) , ( tex2DNode27_g275.g * -1.0 )));
				float4 tex2DNode28_g275 = tex2D( _Layer03Normal, (temp_output_68_0_g275).yz );
				float temp_output_51_0_g275 = (temp_output_54_0_g275).x;
				float3 appendResult36_g275 = (float3(( tex2DNode28_g275.r * -1.0 ) , ( tex2DNode28_g275.b * temp_output_51_0_g275 ) , ( tex2DNode28_g275.g * -1.0 )));
				float temp_output_1_0_g276 = 0.0;
				float lerpResult5_g276 = lerp( ( 0.0 - temp_output_1_0_g276 ) , ( temp_output_1_0_g276 + 1.0 ) , abs( temp_output_51_0_g275 ));
				float temp_output_58_0_g275 = saturate( lerpResult5_g276 );
				float3 lerpResult24_g275 = lerp( appendResult35_g275 , appendResult36_g275 , temp_output_58_0_g275);
				float4 tex2DNode29_g275 = tex2D( _Layer03Normal, (temp_output_68_0_g275).xy );
				float temp_output_53_0_g275 = (temp_output_54_0_g275).z;
				float3 appendResult50_g275 = (float3(( (tex2DNode29_g275).rg * float2( -1,-1 ) ) , ( tex2DNode29_g275.b * temp_output_53_0_g275 )));
				float temp_output_1_0_g288 = 0.5;
				float lerpResult5_g288 = lerp( ( 0.0 - temp_output_1_0_g288 ) , ( temp_output_1_0_g288 + 1.0 ) , abs( temp_output_53_0_g275 ));
				float temp_output_59_0_g275 = saturate( lerpResult5_g288 );
				float3 lerpResult25_g275 = lerp( lerpResult24_g275 , appendResult50_g275 , temp_output_59_0_g275);
				float3 normalizeResult20_g275 = normalize( lerpResult25_g275 );
				float3 temp_output_22_0_g277 = WorldNormal;
				float3 normalizeResult6_g280 = normalize( temp_output_22_0_g277 );
				float3 normalizeResult7_g280 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g280 = cross( normalizeResult6_g280 , normalizeResult7_g280 );
				float3 temp_output_10_0_g281 = temp_output_5_0_g280;
				float dotResult5_g281 = dot( temp_output_10_0_g281 , temp_output_10_0_g281 );
				float3 normalizeResult7_g281 = normalize( temp_output_10_0_g281 );
				float2 appendResult8_g281 = (float2(normalizeResult7_g281.x , 0.0));
				float2 appendResult11_g281 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g281 = 0;
				if( dotResult5_g281 <= 1E-06 )
				ifLocalVar4_g281 = appendResult11_g281;
				else
				ifLocalVar4_g281 = appendResult8_g281;
				float3 temp_cast_68 = (temp_output_69_0_g275).xxx;
				float3 temp_output_59_0_g277 = ( temp_output_72_0_g275 / temp_cast_68 );
				float dotResult49_g277 = dot( temp_output_22_0_g277 , float3(0,1,0) );
				float ifLocalVar45_g277 = 0;
				if( dotResult49_g277 > 0.0 )
				ifLocalVar45_g277 = -1.0;
				else if( dotResult49_g277 < 0.0 )
				ifLocalVar45_g277 = 1.0;
				float3 appendResult38_g277 = (float3(ifLocalVar45_g277 , -1.0 , 1.0));
				float dotResult54_g277 = dot( temp_output_22_0_g277 , float3(1,0,0) );
				float ifLocalVar50_g277 = 0;
				if( dotResult54_g277 > 0.0 )
				ifLocalVar50_g277 = 1.0;
				else if( dotResult54_g277 < 0.0 )
				ifLocalVar50_g277 = -1.0;
				float3 appendResult37_g277 = (float3(ifLocalVar50_g277 , -1.0 , 1.0));
				float4 lerpResult27_g277 = lerp( ( tex2D( _Layer03Normal, (temp_output_59_0_g277).xz ) * float4( appendResult38_g277 , 0.0 ) ) , ( tex2D( _Layer03Normal, (temp_output_59_0_g277).yz ) * float4( appendResult37_g277 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g275 ));
				float3 break5_g284 = lerpResult27_g277.rgb;
				float3 temp_output_10_0_g282 = cross( temp_output_5_0_g280 , normalizeResult6_g280 );
				float dotResult5_g282 = dot( temp_output_10_0_g282 , temp_output_10_0_g282 );
				float3 normalizeResult7_g282 = normalize( temp_output_10_0_g282 );
				float2 appendResult8_g282 = (float2(normalizeResult7_g282.x , 0.0));
				float2 appendResult11_g282 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g282 = 0;
				if( dotResult5_g282 <= 1E-06 )
				ifLocalVar4_g282 = appendResult11_g282;
				else
				ifLocalVar4_g282 = appendResult8_g282;
				float3 temp_output_25_0_g277 = ( ( float3( (ifLocalVar4_g281).xy ,  0.0 ) * break5_g284.x ) + ( float3( (ifLocalVar4_g282).xy ,  0.0 ) * break5_g284.y ) + ( normalizeResult6_g280 * break5_g284.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g285 = normalize( temp_output_22_0_g277 );
				float3 normalizeResult7_g285 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g285 = cross( normalizeResult6_g285 , normalizeResult7_g285 );
				float3 temp_output_10_0_g286 = temp_output_5_0_g285;
				float dotResult5_g286 = dot( temp_output_10_0_g286 , temp_output_10_0_g286 );
				float3 normalizeResult7_g286 = normalize( temp_output_10_0_g286 );
				float2 appendResult8_g286 = (float2(normalizeResult7_g286.x , 0.0));
				float2 appendResult11_g286 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g286 = 0;
				if( dotResult5_g286 <= 1E-06 )
				ifLocalVar4_g286 = appendResult11_g286;
				else
				ifLocalVar4_g286 = appendResult8_g286;
				float dotResult20_g277 = dot( temp_output_22_0_g277 , float3(0,0,1) );
				float ifLocalVar16_g277 = 0;
				if( dotResult20_g277 > 0.0 )
				ifLocalVar16_g277 = 1.0;
				else if( dotResult20_g277 < 0.0 )
				ifLocalVar16_g277 = -1.0;
				float3 appendResult14_g277 = (float3(ifLocalVar16_g277 , -1.0 , 1.0));
				float3 break5_g278 = ( tex2D( _Layer03Normal, (temp_output_59_0_g277).xy ) * float4( appendResult14_g277 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g287 = cross( temp_output_5_0_g285 , normalizeResult6_g285 );
				float dotResult5_g287 = dot( temp_output_10_0_g287 , temp_output_10_0_g287 );
				float3 normalizeResult7_g287 = normalize( temp_output_10_0_g287 );
				float2 appendResult8_g287 = (float2(normalizeResult7_g287.x , 0.0));
				float2 appendResult11_g287 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g287 = 0;
				if( dotResult5_g287 <= 1E-06 )
				ifLocalVar4_g287 = appendResult11_g287;
				else
				ifLocalVar4_g287 = appendResult8_g287;
				float3 temp_output_9_0_g277 = ( ( float3( (ifLocalVar4_g286).xy ,  0.0 ) * break5_g278.x ) + ( float3( (ifLocalVar4_g287).xy ,  0.0 ) * break5_g278.y ) + ( normalizeResult6_g285 * break5_g278.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g277 = ( 1.0 - temp_output_59_0_g275 );
				float3 lerpResult5_g277 = lerp( temp_output_25_0_g277 , temp_output_9_0_g277 , temp_output_33_0_g277);
				float3 temp_output_18_0_g275 = ( (float)temp_output_75_0_g275 == 0.0 ? normalizeResult20_g275 : lerpResult5_g277 );
				float3 worldToTangentDir11_g275 = mul( ase_worldToTangent, temp_output_18_0_g275);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch89 = ( (float)temp_output_76_0_g275 == 0.0 ? worldToTangentDir11_g275 : temp_output_18_0_g275 );
				#else
				float3 staticSwitch89 = UnpackNormalScale( tex2D( _Layer03Normal, temp_output_107_0 ), 1.0f );
				#endif
				float3 lerpResult1_g318 = lerp( staticSwitch89 , float3(0,0,1) , _Layer03NormalFlatten);
				float3 lerpResult47 = lerp( lerpResult46 , lerpResult1_g318 , temp_output_51_0);
				int temp_output_76_0_g294 = 0;
				int temp_output_75_0_g294 = (int)1.0;
				float3 temp_output_72_0_g294 = WorldPosition;
				float temp_output_69_0_g294 = ( abs( ( _Layer04Tiling * 512.0 ) ) * -1.0 );
				float3 temp_output_68_0_g294 = ( temp_output_72_0_g294 / temp_output_69_0_g294 );
				float4 tex2DNode27_g294 = tex2D( _Layer04Normal, (temp_output_68_0_g294).xz );
				float3 tangentToWorldDir63_g294 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g294 = ( tangentToWorldDir63_g294 * float3( 1,0,0 ) );
				float3 appendResult35_g294 = (float3(( tex2DNode27_g294.r * -1.0 ) , ( tex2DNode27_g294.b * (temp_output_54_0_g294).y ) , ( tex2DNode27_g294.g * -1.0 )));
				float4 tex2DNode28_g294 = tex2D( _Layer04Normal, (temp_output_68_0_g294).yz );
				float temp_output_51_0_g294 = (temp_output_54_0_g294).x;
				float3 appendResult36_g294 = (float3(( tex2DNode28_g294.r * -1.0 ) , ( tex2DNode28_g294.b * temp_output_51_0_g294 ) , ( tex2DNode28_g294.g * -1.0 )));
				float temp_output_1_0_g295 = 0.0;
				float lerpResult5_g295 = lerp( ( 0.0 - temp_output_1_0_g295 ) , ( temp_output_1_0_g295 + 1.0 ) , abs( temp_output_51_0_g294 ));
				float temp_output_58_0_g294 = saturate( lerpResult5_g295 );
				float3 lerpResult24_g294 = lerp( appendResult35_g294 , appendResult36_g294 , temp_output_58_0_g294);
				float4 tex2DNode29_g294 = tex2D( _Layer04Normal, (temp_output_68_0_g294).xy );
				float temp_output_53_0_g294 = (temp_output_54_0_g294).z;
				float3 appendResult50_g294 = (float3(( (tex2DNode29_g294).rg * float2( -1,-1 ) ) , ( tex2DNode29_g294.b * temp_output_53_0_g294 )));
				float temp_output_1_0_g307 = 0.5;
				float lerpResult5_g307 = lerp( ( 0.0 - temp_output_1_0_g307 ) , ( temp_output_1_0_g307 + 1.0 ) , abs( temp_output_53_0_g294 ));
				float temp_output_59_0_g294 = saturate( lerpResult5_g307 );
				float3 lerpResult25_g294 = lerp( lerpResult24_g294 , appendResult50_g294 , temp_output_59_0_g294);
				float3 normalizeResult20_g294 = normalize( lerpResult25_g294 );
				float3 temp_output_22_0_g296 = WorldNormal;
				float3 normalizeResult6_g299 = normalize( temp_output_22_0_g296 );
				float3 normalizeResult7_g299 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g299 = cross( normalizeResult6_g299 , normalizeResult7_g299 );
				float3 temp_output_10_0_g300 = temp_output_5_0_g299;
				float dotResult5_g300 = dot( temp_output_10_0_g300 , temp_output_10_0_g300 );
				float3 normalizeResult7_g300 = normalize( temp_output_10_0_g300 );
				float2 appendResult8_g300 = (float2(normalizeResult7_g300.x , 0.0));
				float2 appendResult11_g300 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g300 = 0;
				if( dotResult5_g300 <= 1E-06 )
				ifLocalVar4_g300 = appendResult11_g300;
				else
				ifLocalVar4_g300 = appendResult8_g300;
				float3 temp_cast_90 = (temp_output_69_0_g294).xxx;
				float3 temp_output_59_0_g296 = ( temp_output_72_0_g294 / temp_cast_90 );
				float dotResult49_g296 = dot( temp_output_22_0_g296 , float3(0,1,0) );
				float ifLocalVar45_g296 = 0;
				if( dotResult49_g296 > 0.0 )
				ifLocalVar45_g296 = -1.0;
				else if( dotResult49_g296 < 0.0 )
				ifLocalVar45_g296 = 1.0;
				float3 appendResult38_g296 = (float3(ifLocalVar45_g296 , -1.0 , 1.0));
				float dotResult54_g296 = dot( temp_output_22_0_g296 , float3(1,0,0) );
				float ifLocalVar50_g296 = 0;
				if( dotResult54_g296 > 0.0 )
				ifLocalVar50_g296 = 1.0;
				else if( dotResult54_g296 < 0.0 )
				ifLocalVar50_g296 = -1.0;
				float3 appendResult37_g296 = (float3(ifLocalVar50_g296 , -1.0 , 1.0));
				float4 lerpResult27_g296 = lerp( ( tex2D( _Layer04Normal, (temp_output_59_0_g296).xz ) * float4( appendResult38_g296 , 0.0 ) ) , ( tex2D( _Layer04Normal, (temp_output_59_0_g296).yz ) * float4( appendResult37_g296 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g294 ));
				float3 break5_g303 = lerpResult27_g296.rgb;
				float3 temp_output_10_0_g301 = cross( temp_output_5_0_g299 , normalizeResult6_g299 );
				float dotResult5_g301 = dot( temp_output_10_0_g301 , temp_output_10_0_g301 );
				float3 normalizeResult7_g301 = normalize( temp_output_10_0_g301 );
				float2 appendResult8_g301 = (float2(normalizeResult7_g301.x , 0.0));
				float2 appendResult11_g301 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g301 = 0;
				if( dotResult5_g301 <= 1E-06 )
				ifLocalVar4_g301 = appendResult11_g301;
				else
				ifLocalVar4_g301 = appendResult8_g301;
				float3 temp_output_25_0_g296 = ( ( float3( (ifLocalVar4_g300).xy ,  0.0 ) * break5_g303.x ) + ( float3( (ifLocalVar4_g301).xy ,  0.0 ) * break5_g303.y ) + ( normalizeResult6_g299 * break5_g303.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g304 = normalize( temp_output_22_0_g296 );
				float3 normalizeResult7_g304 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g304 = cross( normalizeResult6_g304 , normalizeResult7_g304 );
				float3 temp_output_10_0_g305 = temp_output_5_0_g304;
				float dotResult5_g305 = dot( temp_output_10_0_g305 , temp_output_10_0_g305 );
				float3 normalizeResult7_g305 = normalize( temp_output_10_0_g305 );
				float2 appendResult8_g305 = (float2(normalizeResult7_g305.x , 0.0));
				float2 appendResult11_g305 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g305 = 0;
				if( dotResult5_g305 <= 1E-06 )
				ifLocalVar4_g305 = appendResult11_g305;
				else
				ifLocalVar4_g305 = appendResult8_g305;
				float dotResult20_g296 = dot( temp_output_22_0_g296 , float3(0,0,1) );
				float ifLocalVar16_g296 = 0;
				if( dotResult20_g296 > 0.0 )
				ifLocalVar16_g296 = 1.0;
				else if( dotResult20_g296 < 0.0 )
				ifLocalVar16_g296 = -1.0;
				float3 appendResult14_g296 = (float3(ifLocalVar16_g296 , -1.0 , 1.0));
				float3 break5_g297 = ( tex2D( _Layer04Normal, (temp_output_59_0_g296).xy ) * float4( appendResult14_g296 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g306 = cross( temp_output_5_0_g304 , normalizeResult6_g304 );
				float dotResult5_g306 = dot( temp_output_10_0_g306 , temp_output_10_0_g306 );
				float3 normalizeResult7_g306 = normalize( temp_output_10_0_g306 );
				float2 appendResult8_g306 = (float2(normalizeResult7_g306.x , 0.0));
				float2 appendResult11_g306 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g306 = 0;
				if( dotResult5_g306 <= 1E-06 )
				ifLocalVar4_g306 = appendResult11_g306;
				else
				ifLocalVar4_g306 = appendResult8_g306;
				float3 temp_output_9_0_g296 = ( ( float3( (ifLocalVar4_g305).xy ,  0.0 ) * break5_g297.x ) + ( float3( (ifLocalVar4_g306).xy ,  0.0 ) * break5_g297.y ) + ( normalizeResult6_g304 * break5_g297.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g296 = ( 1.0 - temp_output_59_0_g294 );
				float3 lerpResult5_g296 = lerp( temp_output_25_0_g296 , temp_output_9_0_g296 , temp_output_33_0_g296);
				float3 temp_output_18_0_g294 = ( (float)temp_output_75_0_g294 == 0.0 ? normalizeResult20_g294 : lerpResult5_g296 );
				float3 worldToTangentDir11_g294 = mul( ase_worldToTangent, temp_output_18_0_g294);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch90 = ( (float)temp_output_76_0_g294 == 0.0 ? worldToTangentDir11_g294 : temp_output_18_0_g294 );
				#else
				float3 staticSwitch90 = UnpackNormalScale( tex2D( _Layer04Normal, temp_output_108_0 ), 1.0f );
				#endif
				float3 lerpResult1_g319 = lerp( staticSwitch90 , float3(0,0,1) , _Layer04NormalFlatten);
				float3 lerpResult48 = lerp( lerpResult47 , lerpResult1_g319 , temp_output_52_0);
				float3 lerpResult19 = lerp( lerpResult48 , staticSwitch91 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float3 staticSwitch12 = lerpResult19;
				#else
				float3 staticSwitch12 = lerpResult48;
				#endif
				float2 texCoord10 = IN.ase_texcoord7.xy * float2( 5,5 ) + float2( 0,0 );
				
				float lerpResult38 = lerp( _Layer1Spec , _Layer2Spec , temp_output_50_0);
				float lerpResult39 = lerp( lerpResult38 , _Layer3Spec , temp_output_51_0);
				float lerpResult40 = lerp( lerpResult39 , _Layer4Spec , temp_output_52_0);
				float3 temp_cast_106 = (lerpResult40).xxx;
				
				float4 lerpResult28 = lerp( staticSwitch82 , staticSwitch83 , temp_output_50_0);
				float4 lerpResult29 = lerp( lerpResult28 , staticSwitch84 , temp_output_51_0);
				float4 lerpResult30 = lerp( lerpResult29 , staticSwitch85 , temp_output_52_0);
				float lerpResult33 = lerp( _RoughnessMin , _RoughnessMax , (lerpResult30).r);
				float lerpResult18 = lerp( lerpResult33 , temp_output_74_0 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float staticSwitch13 = lerpResult18;
				#else
				float staticSwitch13 = lerpResult33;
				#endif
				
				float3 Albedo = staticSwitch15.rgb;
				float3 Normal = ( staticSwitch12 + UnpackNormalScale( tex2D( _TerrainNormal, texCoord10 ), 1.0f ) );
				float3 Emission = 0;
				float3 Specular = temp_cast_106;
				float Metallic = 0;
				float Smoothness = ( 1.0 - staticSwitch13 );
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif
				
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(IN.clipPos);
				inputData.shadowMask = SAMPLE_SHADOWMASK(IN.lightmapUVOrVertexSH.xy);

				half4 color = UniversalFragmentPBR(
					inputData, 
					Albedo, 
					Metallic, 
					Specular, 
					Smoothness, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif

				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );

					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif

				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, float4( WorldNormal,0 ) ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos.xy ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif

				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				return color;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag
#if ASE_SRP_VERSION >= 110000
			#pragma multi_compile _ _CASTING_PUNCTUAL_LIGHT_SHADOW
#endif
			#define SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Layer01AColor;
			float4 _Layer04BColor;
			float4 _Layer04AColor;
			float4 _Layer03AColor;
			float4 _Layer02BColor;
			float4 _Layer02AColor;
			float4 _Layer03BColor;
			float4 _Layer01BColor;
			float _FlowBlend;
			float _Layer4Spec;
			float _Layer3Spec;
			float _Layer2Spec;
			float _Layer1Spec;
			float _Layer04NormalFlatten;
			float _Layer03NormalFlatten;
			float _Layer02NormalFlatten;
			float _Layer01NormalFlatten;
			float _DistanceBlendContrast;
			float _Blend03Bias;
			float _Blend03Contrast;
			float _Layer04Tiling;
			float _FlowStrength;
			float _UVWorldXYScale;
			float _Blend02Contrast;
			float _Layer03Tiling;
			float _RoughnessMin;
			float _DISTANCE_FIELD_NOT_SUPPORTED;
			float _Blend01Bias;
			float _BlendsDivisor;
			float _BlendsOffset;
			float _Blend01Contrast;
			float _Layer02Tiling;
			float _DistanceFieldUVScale;
			float _FlowUVScale;
			float _Layer01Tiling;
			float _Layer01FlowIntensity;
			float _Blend02Bias;
			float _RoughnessMax;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			float3 _LightDirection;
#if ASE_SRP_VERSION >= 110000 
			float3 _LightPosition;
#endif
			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				float3 normalWS = TransformObjectToWorldDir(v.ase_normal);

		#if ASE_SRP_VERSION >= 110000 
			#if _CASTING_PUNCTUAL_LIGHT_SHADOW
				float3 lightDirectionWS = normalize(_LightPosition - positionWS);
			#else
				float3 lightDirectionWS = _LightDirection;
			#endif
				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));
			#if UNITY_REVERSED_Z
				clipPos.z = min(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#else
				clipPos.z = max(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#endif
		#else
				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, _LightDirection));
			#if UNITY_REVERSED_Z
				clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
			#else
				clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
			#endif
		#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = clipPos;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );
				
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				return 0;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Layer01AColor;
			float4 _Layer04BColor;
			float4 _Layer04AColor;
			float4 _Layer03AColor;
			float4 _Layer02BColor;
			float4 _Layer02AColor;
			float4 _Layer03BColor;
			float4 _Layer01BColor;
			float _FlowBlend;
			float _Layer4Spec;
			float _Layer3Spec;
			float _Layer2Spec;
			float _Layer1Spec;
			float _Layer04NormalFlatten;
			float _Layer03NormalFlatten;
			float _Layer02NormalFlatten;
			float _Layer01NormalFlatten;
			float _DistanceBlendContrast;
			float _Blend03Bias;
			float _Blend03Contrast;
			float _Layer04Tiling;
			float _FlowStrength;
			float _UVWorldXYScale;
			float _Blend02Contrast;
			float _Layer03Tiling;
			float _RoughnessMin;
			float _DISTANCE_FIELD_NOT_SUPPORTED;
			float _Blend01Bias;
			float _BlendsDivisor;
			float _BlendsOffset;
			float _Blend01Contrast;
			float _Layer02Tiling;
			float _DistanceFieldUVScale;
			float _FlowUVScale;
			float _Layer01Tiling;
			float _Layer01FlowIntensity;
			float _Blend02Bias;
			float _RoughnessMax;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif

				return 0;
			}
			ENDHLSL
		}
		
		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#pragma shader_feature_local _BLENDON_ON
			#pragma shader_feature_local _SWITCHTOWORLDTRIPLANAR_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Layer01AColor;
			float4 _Layer04BColor;
			float4 _Layer04AColor;
			float4 _Layer03AColor;
			float4 _Layer02BColor;
			float4 _Layer02AColor;
			float4 _Layer03BColor;
			float4 _Layer01BColor;
			float _FlowBlend;
			float _Layer4Spec;
			float _Layer3Spec;
			float _Layer2Spec;
			float _Layer1Spec;
			float _Layer04NormalFlatten;
			float _Layer03NormalFlatten;
			float _Layer02NormalFlatten;
			float _Layer01NormalFlatten;
			float _DistanceBlendContrast;
			float _Blend03Bias;
			float _Blend03Contrast;
			float _Layer04Tiling;
			float _FlowStrength;
			float _UVWorldXYScale;
			float _Blend02Contrast;
			float _Layer03Tiling;
			float _RoughnessMin;
			float _DISTANCE_FIELD_NOT_SUPPORTED;
			float _Blend01Bias;
			float _BlendsDivisor;
			float _BlendsOffset;
			float _Blend01Contrast;
			float _Layer02Tiling;
			float _DistanceFieldUVScale;
			float _FlowUVScale;
			float _Layer01Tiling;
			float _Layer01FlowIntensity;
			float _Blend02Bias;
			float _RoughnessMax;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _Layer01RAOHMTexture;
			sampler2D _Texture3;
			sampler2D _Layer02RAOHMTexture;
			sampler2D _Layer03RAOHMTexture;
			sampler2D _Layer04RAOHMTexture;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord2.xyz = ase_worldNormal;
				
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 temp_output_131_0 = (( WorldPosition / ( _UVWorldXYScale * 512.0 ) )).xy;
				float2 temp_output_21_0_g83 = temp_output_131_0;
				float temp_output_11_0_g83 = 0.0;
				float3 temp_cast_0 = (( _DISTANCE_FIELD_NOT_SUPPORTED / _DistanceFieldUVScale )).xxx;
				float3 worldToObjDir143 = mul( GetWorldToObjectMatrix(), float4( temp_cast_0, 0 ) ).xyz;
				float2 normalizeResult138 = normalize( ( ( (worldToObjDir143).xy + ( (tex2D( _Texture3, (( WorldPosition / ( _FlowUVScale * 512.0 ) )).xy )).rg / float2( 32,0 ) ) ) - float2( 0.5,0 ) ) );
				float2 lerpResult134 = lerp( temp_output_131_0 , ( normalizeResult138 * _FlowStrength ) , _FlowBlend);
				float2 temp_output_15_0_g83 = ( lerpResult134 * float2( -1,1 ) );
				float2 temp_output_16_0_g83 = ( frac( ( temp_output_11_0_g83 - 0.5 ) ) * temp_output_15_0_g83 );
				float2 temp_output_19_0_g83 = ( temp_output_21_0_g83 + temp_output_16_0_g83 );
				float temp_output_12_0_g83 = frac( temp_output_11_0_g83 );
				float2 temp_output_13_0_g83 = ( temp_output_15_0_g83 * temp_output_12_0_g83 );
				float2 temp_output_22_0_g83 = ( temp_output_13_0_g83 + temp_output_21_0_g83 );
				float temp_output_2_0_g85 = temp_output_12_0_g83;
				float temp_output_3_0_g85 = 1.0;
				float temp_output_4_0_g85 = ( temp_output_2_0_g85 + ( 0.25 * temp_output_3_0_g85 ) );
				float ifLocalVar24_g85 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g85 = temp_output_2_0_g85;
				else
				ifLocalVar24_g85 = temp_output_4_0_g85;
				float temp_output_7_0_g85 = frac( ( ifLocalVar24_g85 / temp_output_3_0_g85 ) );
				float temp_output_8_0_g85 = ( 2.0 * temp_output_7_0_g85 );
				float temp_output_12_0_g85 = floor( temp_output_8_0_g85 );
				float lerpResult13_g85 = lerp( temp_output_8_0_g85 , ( 2.0 * ( 1.0 - temp_output_7_0_g85 ) ) , temp_output_12_0_g85);
				float temp_output_78_0_g83 = lerpResult13_g85;
				float2 lerpResult60_g83 = lerp( temp_output_19_0_g83 , temp_output_22_0_g83 , temp_output_78_0_g83);
				float2 temp_output_124_62 = lerpResult60_g83;
				float2 lerpResult114 = lerp( temp_output_131_0 , temp_output_124_62 , _Layer01FlowIntensity);
				float2 temp_output_105_0 = ( lerpResult114 * _Layer01Tiling );
				float temp_output_23_0_g94 = 0.0;
				float3 temp_cast_1 = (( _Layer01Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g94 = ( WorldPosition / ( abs( temp_cast_1 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xz );
				float4 ifLocalVar24_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar24_g94 = float4( (tex2DNode31_g94).rgb , 0.0 );
				else
				ifLocalVar24_g94 = tex2DNode31_g94;
				float4 tex2DNode29_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).yz );
				float4 ifLocalVar21_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar21_g94 = float4( (tex2DNode29_g94).rgb , 0.0 );
				else
				ifLocalVar21_g94 = tex2DNode29_g94;
				float temp_output_9_0_g94 = 0.5;
				float temp_output_1_0_g95 = temp_output_9_0_g94;
				float3 ase_worldNormal = IN.ase_texcoord2.xyz;
				float3 normalizedWorldNormal = normalize( ase_worldNormal );
				float3 temp_output_15_0_g94 = normalizedWorldNormal;
				float lerpResult5_g95 = lerp( ( 0.0 - temp_output_1_0_g95 ) , ( temp_output_1_0_g95 + 1.0 ) , abs( (temp_output_15_0_g94).x ));
				float4 lerpResult3_g94 = lerp( ifLocalVar24_g94 , ifLocalVar21_g94 , saturate( lerpResult5_g95 ));
				float4 tex2DNode32_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xy );
				float4 ifLocalVar25_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar25_g94 = float4( (tex2DNode32_g94).rgb , 0.0 );
				else
				ifLocalVar25_g94 = tex2DNode32_g94;
				float temp_output_1_0_g96 = temp_output_9_0_g94;
				float lerpResult5_g96 = lerp( ( 0.0 - temp_output_1_0_g96 ) , ( temp_output_1_0_g96 + 1.0 ) , abs( (temp_output_15_0_g94).z ));
				float4 lerpResult4_g94 = lerp( lerpResult3_g94 , ifLocalVar25_g94 , saturate( lerpResult5_g96 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch82 = lerpResult4_g94;
				#else
				float4 staticSwitch82 = tex2D( _Layer01RAOHMTexture, temp_output_105_0 );
				#endif
				float temp_output_74_0 = ( 1.0 - (staticSwitch82).b );
				float4 lerpResult59 = lerp( _Layer01AColor , _Layer01BColor , temp_output_74_0);
				float2 lerpResult115 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_106_0 = ( lerpResult115 * _Layer02Tiling );
				float temp_output_23_0_g91 = 0.0;
				float3 temp_cast_5 = (( _Layer02Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g91 = ( WorldPosition / ( abs( temp_cast_5 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xz );
				float4 ifLocalVar24_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar24_g91 = float4( (tex2DNode31_g91).rgb , 0.0 );
				else
				ifLocalVar24_g91 = tex2DNode31_g91;
				float4 tex2DNode29_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).yz );
				float4 ifLocalVar21_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar21_g91 = float4( (tex2DNode29_g91).rgb , 0.0 );
				else
				ifLocalVar21_g91 = tex2DNode29_g91;
				float temp_output_9_0_g91 = 0.5;
				float temp_output_1_0_g92 = temp_output_9_0_g91;
				float3 temp_output_15_0_g91 = normalizedWorldNormal;
				float lerpResult5_g92 = lerp( ( 0.0 - temp_output_1_0_g92 ) , ( temp_output_1_0_g92 + 1.0 ) , abs( (temp_output_15_0_g91).x ));
				float4 lerpResult3_g91 = lerp( ifLocalVar24_g91 , ifLocalVar21_g91 , saturate( lerpResult5_g92 ));
				float4 tex2DNode32_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xy );
				float4 ifLocalVar25_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar25_g91 = float4( (tex2DNode32_g91).rgb , 0.0 );
				else
				ifLocalVar25_g91 = tex2DNode32_g91;
				float temp_output_1_0_g93 = temp_output_9_0_g91;
				float lerpResult5_g93 = lerp( ( 0.0 - temp_output_1_0_g93 ) , ( temp_output_1_0_g93 + 1.0 ) , abs( (temp_output_15_0_g91).z ));
				float4 lerpResult4_g91 = lerp( lerpResult3_g91 , ifLocalVar25_g91 , saturate( lerpResult5_g93 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch83 = lerpResult4_g91;
				#else
				float4 staticSwitch83 = tex2D( _Layer02RAOHMTexture, temp_output_106_0 );
				#endif
				float temp_output_77_0 = ( 1.0 - (staticSwitch83).b );
				float4 lerpResult60 = lerp( _Layer02AColor , _Layer02BColor , temp_output_77_0);
				float temp_output_1_0_g246 = _Blend01Contrast;
				float temp_output_2_0_g245 = temp_output_74_0;
				float4 break196 = ( ( IN.ase_color + _BlendsOffset ) / _BlendsDivisor );
				float temp_output_1_0_g245 = pow( ( 1.0 - saturate( break196.r ) ) , _Blend01Bias );
				float temp_output_3_0_g245 = temp_output_77_0;
				float temp_output_17_0_g245 = saturate( ( ( ( temp_output_2_0_g245 + ( 1.0 - temp_output_1_0_g245 ) ) + 1.0 ) - ( ( temp_output_1_0_g245 * 2.0 ) + temp_output_3_0_g245 ) ) );
				float lerpResult5_g246 = lerp( ( 0.0 - temp_output_1_0_g246 ) , ( temp_output_1_0_g246 + 1.0 ) , temp_output_17_0_g245);
				float temp_output_24_0_g245 = saturate( lerpResult5_g246 );
				float temp_output_50_0 = temp_output_24_0_g245;
				float4 lerpResult56 = lerp( lerpResult59 , lerpResult60 , temp_output_50_0);
				float2 lerpResult116 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_107_0 = ( lerpResult116 * _Layer03Tiling );
				float temp_output_23_0_g97 = 0.0;
				float3 temp_cast_9 = (( _Layer03Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g97 = ( WorldPosition / ( abs( temp_cast_9 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xz );
				float4 ifLocalVar24_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar24_g97 = float4( (tex2DNode31_g97).rgb , 0.0 );
				else
				ifLocalVar24_g97 = tex2DNode31_g97;
				float4 tex2DNode29_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).yz );
				float4 ifLocalVar21_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar21_g97 = float4( (tex2DNode29_g97).rgb , 0.0 );
				else
				ifLocalVar21_g97 = tex2DNode29_g97;
				float temp_output_9_0_g97 = 0.5;
				float temp_output_1_0_g98 = temp_output_9_0_g97;
				float3 temp_output_15_0_g97 = normalizedWorldNormal;
				float lerpResult5_g98 = lerp( ( 0.0 - temp_output_1_0_g98 ) , ( temp_output_1_0_g98 + 1.0 ) , abs( (temp_output_15_0_g97).x ));
				float4 lerpResult3_g97 = lerp( ifLocalVar24_g97 , ifLocalVar21_g97 , saturate( lerpResult5_g98 ));
				float4 tex2DNode32_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xy );
				float4 ifLocalVar25_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar25_g97 = float4( (tex2DNode32_g97).rgb , 0.0 );
				else
				ifLocalVar25_g97 = tex2DNode32_g97;
				float temp_output_1_0_g99 = temp_output_9_0_g97;
				float lerpResult5_g99 = lerp( ( 0.0 - temp_output_1_0_g99 ) , ( temp_output_1_0_g99 + 1.0 ) , abs( (temp_output_15_0_g97).z ));
				float4 lerpResult4_g97 = lerp( lerpResult3_g97 , ifLocalVar25_g97 , saturate( lerpResult5_g99 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch84 = lerpResult4_g97;
				#else
				float4 staticSwitch84 = tex2D( _Layer03RAOHMTexture, temp_output_107_0 );
				#endif
				float temp_output_79_0 = ( 1.0 - (staticSwitch84).b );
				float4 lerpResult61 = lerp( _Layer03AColor , _Layer03BColor , temp_output_79_0);
				float temp_output_1_0_g290 = _Blend02Contrast;
				float lerpResult16_g245 = lerp( temp_output_3_0_g245 , temp_output_2_0_g245 , temp_output_24_0_g245);
				float temp_output_2_0_g289 = lerpResult16_g245;
				float temp_output_1_0_g289 = pow( ( 1.0 - saturate( break196.g ) ) , _Blend02Bias );
				float temp_output_3_0_g289 = temp_output_79_0;
				float temp_output_17_0_g289 = saturate( ( ( ( temp_output_2_0_g289 + ( 1.0 - temp_output_1_0_g289 ) ) + 1.0 ) - ( ( temp_output_1_0_g289 * 2.0 ) + temp_output_3_0_g289 ) ) );
				float lerpResult5_g290 = lerp( ( 0.0 - temp_output_1_0_g290 ) , ( temp_output_1_0_g290 + 1.0 ) , temp_output_17_0_g289);
				float temp_output_24_0_g289 = saturate( lerpResult5_g290 );
				float temp_output_51_0 = temp_output_24_0_g289;
				float4 lerpResult57 = lerp( lerpResult56 , lerpResult61 , temp_output_51_0);
				float2 lerpResult117 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_108_0 = ( lerpResult117 * _Layer04Tiling );
				float temp_output_23_0_g100 = 0.0;
				float3 temp_cast_13 = (( _Layer04Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g100 = ( WorldPosition / ( abs( temp_cast_13 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xz );
				float4 ifLocalVar24_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar24_g100 = float4( (tex2DNode31_g100).rgb , 0.0 );
				else
				ifLocalVar24_g100 = tex2DNode31_g100;
				float4 tex2DNode29_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).yz );
				float4 ifLocalVar21_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar21_g100 = float4( (tex2DNode29_g100).rgb , 0.0 );
				else
				ifLocalVar21_g100 = tex2DNode29_g100;
				float temp_output_9_0_g100 = 0.5;
				float temp_output_1_0_g101 = temp_output_9_0_g100;
				float3 temp_output_15_0_g100 = normalizedWorldNormal;
				float lerpResult5_g101 = lerp( ( 0.0 - temp_output_1_0_g101 ) , ( temp_output_1_0_g101 + 1.0 ) , abs( (temp_output_15_0_g100).x ));
				float4 lerpResult3_g100 = lerp( ifLocalVar24_g100 , ifLocalVar21_g100 , saturate( lerpResult5_g101 ));
				float4 tex2DNode32_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xy );
				float4 ifLocalVar25_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar25_g100 = float4( (tex2DNode32_g100).rgb , 0.0 );
				else
				ifLocalVar25_g100 = tex2DNode32_g100;
				float temp_output_1_0_g102 = temp_output_9_0_g100;
				float lerpResult5_g102 = lerp( ( 0.0 - temp_output_1_0_g102 ) , ( temp_output_1_0_g102 + 1.0 ) , abs( (temp_output_15_0_g100).z ));
				float4 lerpResult4_g100 = lerp( lerpResult3_g100 , ifLocalVar25_g100 , saturate( lerpResult5_g102 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch85 = lerpResult4_g100;
				#else
				float4 staticSwitch85 = tex2D( _Layer04RAOHMTexture, temp_output_108_0 );
				#endif
				float temp_output_81_0 = ( 1.0 - (staticSwitch85).b );
				float4 lerpResult62 = lerp( _Layer04AColor , _Layer04BColor , temp_output_81_0);
				float temp_output_1_0_g292 = _Blend03Contrast;
				float lerpResult16_g289 = lerp( temp_output_3_0_g289 , temp_output_2_0_g289 , temp_output_24_0_g289);
				float temp_output_2_0_g291 = lerpResult16_g289;
				float temp_output_1_0_g291 = pow( ( 1.0 - saturate( break196.b ) ) , _Blend03Bias );
				float temp_output_3_0_g291 = temp_output_81_0;
				float temp_output_17_0_g291 = saturate( ( ( ( temp_output_2_0_g291 + ( 1.0 - temp_output_1_0_g291 ) ) + 1.0 ) - ( ( temp_output_1_0_g291 * 2.0 ) + temp_output_3_0_g291 ) ) );
				float lerpResult5_g292 = lerp( ( 0.0 - temp_output_1_0_g292 ) , ( temp_output_1_0_g292 + 1.0 ) , temp_output_17_0_g291);
				float temp_output_24_0_g291 = saturate( lerpResult5_g292 );
				float temp_output_52_0 = temp_output_24_0_g291;
				float4 lerpResult58 = lerp( lerpResult57 , lerpResult62 , temp_output_52_0);
				float temp_output_1_0_g311 = _DistanceBlendContrast;
				float lerpResult5_g311 = lerp( ( 0.0 - temp_output_1_0_g311 ) , ( temp_output_1_0_g311 + 1.0 ) , ( ( 20.0 + 0.0 ) / 2.0 ));
				float temp_output_26_0 = saturate( lerpResult5_g311 );
				float4 lerpResult17 = lerp( lerpResult58 , lerpResult62 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float4 staticSwitch15 = lerpResult17;
				#else
				float4 staticSwitch15 = lerpResult58;
				#endif
				
				
				float3 Albedo = staticSwitch15.rgb;
				float3 Emission = 0;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Universal2D"
			Tags { "LightMode"="Universal2D" }

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_2D

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#pragma shader_feature_local _BLENDON_ON
			#pragma shader_feature_local _SWITCHTOWORLDTRIPLANAR_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Layer01AColor;
			float4 _Layer04BColor;
			float4 _Layer04AColor;
			float4 _Layer03AColor;
			float4 _Layer02BColor;
			float4 _Layer02AColor;
			float4 _Layer03BColor;
			float4 _Layer01BColor;
			float _FlowBlend;
			float _Layer4Spec;
			float _Layer3Spec;
			float _Layer2Spec;
			float _Layer1Spec;
			float _Layer04NormalFlatten;
			float _Layer03NormalFlatten;
			float _Layer02NormalFlatten;
			float _Layer01NormalFlatten;
			float _DistanceBlendContrast;
			float _Blend03Bias;
			float _Blend03Contrast;
			float _Layer04Tiling;
			float _FlowStrength;
			float _UVWorldXYScale;
			float _Blend02Contrast;
			float _Layer03Tiling;
			float _RoughnessMin;
			float _DISTANCE_FIELD_NOT_SUPPORTED;
			float _Blend01Bias;
			float _BlendsDivisor;
			float _BlendsOffset;
			float _Blend01Contrast;
			float _Layer02Tiling;
			float _DistanceFieldUVScale;
			float _FlowUVScale;
			float _Layer01Tiling;
			float _Layer01FlowIntensity;
			float _Blend02Bias;
			float _RoughnessMax;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _Layer01RAOHMTexture;
			sampler2D _Texture3;
			sampler2D _Layer02RAOHMTexture;
			sampler2D _Layer03RAOHMTexture;
			sampler2D _Layer04RAOHMTexture;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord2.xyz = ase_worldNormal;
				
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 temp_output_131_0 = (( WorldPosition / ( _UVWorldXYScale * 512.0 ) )).xy;
				float2 temp_output_21_0_g83 = temp_output_131_0;
				float temp_output_11_0_g83 = 0.0;
				float3 temp_cast_0 = (( _DISTANCE_FIELD_NOT_SUPPORTED / _DistanceFieldUVScale )).xxx;
				float3 worldToObjDir143 = mul( GetWorldToObjectMatrix(), float4( temp_cast_0, 0 ) ).xyz;
				float2 normalizeResult138 = normalize( ( ( (worldToObjDir143).xy + ( (tex2D( _Texture3, (( WorldPosition / ( _FlowUVScale * 512.0 ) )).xy )).rg / float2( 32,0 ) ) ) - float2( 0.5,0 ) ) );
				float2 lerpResult134 = lerp( temp_output_131_0 , ( normalizeResult138 * _FlowStrength ) , _FlowBlend);
				float2 temp_output_15_0_g83 = ( lerpResult134 * float2( -1,1 ) );
				float2 temp_output_16_0_g83 = ( frac( ( temp_output_11_0_g83 - 0.5 ) ) * temp_output_15_0_g83 );
				float2 temp_output_19_0_g83 = ( temp_output_21_0_g83 + temp_output_16_0_g83 );
				float temp_output_12_0_g83 = frac( temp_output_11_0_g83 );
				float2 temp_output_13_0_g83 = ( temp_output_15_0_g83 * temp_output_12_0_g83 );
				float2 temp_output_22_0_g83 = ( temp_output_13_0_g83 + temp_output_21_0_g83 );
				float temp_output_2_0_g85 = temp_output_12_0_g83;
				float temp_output_3_0_g85 = 1.0;
				float temp_output_4_0_g85 = ( temp_output_2_0_g85 + ( 0.25 * temp_output_3_0_g85 ) );
				float ifLocalVar24_g85 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g85 = temp_output_2_0_g85;
				else
				ifLocalVar24_g85 = temp_output_4_0_g85;
				float temp_output_7_0_g85 = frac( ( ifLocalVar24_g85 / temp_output_3_0_g85 ) );
				float temp_output_8_0_g85 = ( 2.0 * temp_output_7_0_g85 );
				float temp_output_12_0_g85 = floor( temp_output_8_0_g85 );
				float lerpResult13_g85 = lerp( temp_output_8_0_g85 , ( 2.0 * ( 1.0 - temp_output_7_0_g85 ) ) , temp_output_12_0_g85);
				float temp_output_78_0_g83 = lerpResult13_g85;
				float2 lerpResult60_g83 = lerp( temp_output_19_0_g83 , temp_output_22_0_g83 , temp_output_78_0_g83);
				float2 temp_output_124_62 = lerpResult60_g83;
				float2 lerpResult114 = lerp( temp_output_131_0 , temp_output_124_62 , _Layer01FlowIntensity);
				float2 temp_output_105_0 = ( lerpResult114 * _Layer01Tiling );
				float temp_output_23_0_g94 = 0.0;
				float3 temp_cast_1 = (( _Layer01Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g94 = ( WorldPosition / ( abs( temp_cast_1 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xz );
				float4 ifLocalVar24_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar24_g94 = float4( (tex2DNode31_g94).rgb , 0.0 );
				else
				ifLocalVar24_g94 = tex2DNode31_g94;
				float4 tex2DNode29_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).yz );
				float4 ifLocalVar21_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar21_g94 = float4( (tex2DNode29_g94).rgb , 0.0 );
				else
				ifLocalVar21_g94 = tex2DNode29_g94;
				float temp_output_9_0_g94 = 0.5;
				float temp_output_1_0_g95 = temp_output_9_0_g94;
				float3 ase_worldNormal = IN.ase_texcoord2.xyz;
				float3 normalizedWorldNormal = normalize( ase_worldNormal );
				float3 temp_output_15_0_g94 = normalizedWorldNormal;
				float lerpResult5_g95 = lerp( ( 0.0 - temp_output_1_0_g95 ) , ( temp_output_1_0_g95 + 1.0 ) , abs( (temp_output_15_0_g94).x ));
				float4 lerpResult3_g94 = lerp( ifLocalVar24_g94 , ifLocalVar21_g94 , saturate( lerpResult5_g95 ));
				float4 tex2DNode32_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xy );
				float4 ifLocalVar25_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar25_g94 = float4( (tex2DNode32_g94).rgb , 0.0 );
				else
				ifLocalVar25_g94 = tex2DNode32_g94;
				float temp_output_1_0_g96 = temp_output_9_0_g94;
				float lerpResult5_g96 = lerp( ( 0.0 - temp_output_1_0_g96 ) , ( temp_output_1_0_g96 + 1.0 ) , abs( (temp_output_15_0_g94).z ));
				float4 lerpResult4_g94 = lerp( lerpResult3_g94 , ifLocalVar25_g94 , saturate( lerpResult5_g96 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch82 = lerpResult4_g94;
				#else
				float4 staticSwitch82 = tex2D( _Layer01RAOHMTexture, temp_output_105_0 );
				#endif
				float temp_output_74_0 = ( 1.0 - (staticSwitch82).b );
				float4 lerpResult59 = lerp( _Layer01AColor , _Layer01BColor , temp_output_74_0);
				float2 lerpResult115 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_106_0 = ( lerpResult115 * _Layer02Tiling );
				float temp_output_23_0_g91 = 0.0;
				float3 temp_cast_5 = (( _Layer02Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g91 = ( WorldPosition / ( abs( temp_cast_5 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xz );
				float4 ifLocalVar24_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar24_g91 = float4( (tex2DNode31_g91).rgb , 0.0 );
				else
				ifLocalVar24_g91 = tex2DNode31_g91;
				float4 tex2DNode29_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).yz );
				float4 ifLocalVar21_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar21_g91 = float4( (tex2DNode29_g91).rgb , 0.0 );
				else
				ifLocalVar21_g91 = tex2DNode29_g91;
				float temp_output_9_0_g91 = 0.5;
				float temp_output_1_0_g92 = temp_output_9_0_g91;
				float3 temp_output_15_0_g91 = normalizedWorldNormal;
				float lerpResult5_g92 = lerp( ( 0.0 - temp_output_1_0_g92 ) , ( temp_output_1_0_g92 + 1.0 ) , abs( (temp_output_15_0_g91).x ));
				float4 lerpResult3_g91 = lerp( ifLocalVar24_g91 , ifLocalVar21_g91 , saturate( lerpResult5_g92 ));
				float4 tex2DNode32_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xy );
				float4 ifLocalVar25_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar25_g91 = float4( (tex2DNode32_g91).rgb , 0.0 );
				else
				ifLocalVar25_g91 = tex2DNode32_g91;
				float temp_output_1_0_g93 = temp_output_9_0_g91;
				float lerpResult5_g93 = lerp( ( 0.0 - temp_output_1_0_g93 ) , ( temp_output_1_0_g93 + 1.0 ) , abs( (temp_output_15_0_g91).z ));
				float4 lerpResult4_g91 = lerp( lerpResult3_g91 , ifLocalVar25_g91 , saturate( lerpResult5_g93 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch83 = lerpResult4_g91;
				#else
				float4 staticSwitch83 = tex2D( _Layer02RAOHMTexture, temp_output_106_0 );
				#endif
				float temp_output_77_0 = ( 1.0 - (staticSwitch83).b );
				float4 lerpResult60 = lerp( _Layer02AColor , _Layer02BColor , temp_output_77_0);
				float temp_output_1_0_g246 = _Blend01Contrast;
				float temp_output_2_0_g245 = temp_output_74_0;
				float4 break196 = ( ( IN.ase_color + _BlendsOffset ) / _BlendsDivisor );
				float temp_output_1_0_g245 = pow( ( 1.0 - saturate( break196.r ) ) , _Blend01Bias );
				float temp_output_3_0_g245 = temp_output_77_0;
				float temp_output_17_0_g245 = saturate( ( ( ( temp_output_2_0_g245 + ( 1.0 - temp_output_1_0_g245 ) ) + 1.0 ) - ( ( temp_output_1_0_g245 * 2.0 ) + temp_output_3_0_g245 ) ) );
				float lerpResult5_g246 = lerp( ( 0.0 - temp_output_1_0_g246 ) , ( temp_output_1_0_g246 + 1.0 ) , temp_output_17_0_g245);
				float temp_output_24_0_g245 = saturate( lerpResult5_g246 );
				float temp_output_50_0 = temp_output_24_0_g245;
				float4 lerpResult56 = lerp( lerpResult59 , lerpResult60 , temp_output_50_0);
				float2 lerpResult116 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_107_0 = ( lerpResult116 * _Layer03Tiling );
				float temp_output_23_0_g97 = 0.0;
				float3 temp_cast_9 = (( _Layer03Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g97 = ( WorldPosition / ( abs( temp_cast_9 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xz );
				float4 ifLocalVar24_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar24_g97 = float4( (tex2DNode31_g97).rgb , 0.0 );
				else
				ifLocalVar24_g97 = tex2DNode31_g97;
				float4 tex2DNode29_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).yz );
				float4 ifLocalVar21_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar21_g97 = float4( (tex2DNode29_g97).rgb , 0.0 );
				else
				ifLocalVar21_g97 = tex2DNode29_g97;
				float temp_output_9_0_g97 = 0.5;
				float temp_output_1_0_g98 = temp_output_9_0_g97;
				float3 temp_output_15_0_g97 = normalizedWorldNormal;
				float lerpResult5_g98 = lerp( ( 0.0 - temp_output_1_0_g98 ) , ( temp_output_1_0_g98 + 1.0 ) , abs( (temp_output_15_0_g97).x ));
				float4 lerpResult3_g97 = lerp( ifLocalVar24_g97 , ifLocalVar21_g97 , saturate( lerpResult5_g98 ));
				float4 tex2DNode32_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xy );
				float4 ifLocalVar25_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar25_g97 = float4( (tex2DNode32_g97).rgb , 0.0 );
				else
				ifLocalVar25_g97 = tex2DNode32_g97;
				float temp_output_1_0_g99 = temp_output_9_0_g97;
				float lerpResult5_g99 = lerp( ( 0.0 - temp_output_1_0_g99 ) , ( temp_output_1_0_g99 + 1.0 ) , abs( (temp_output_15_0_g97).z ));
				float4 lerpResult4_g97 = lerp( lerpResult3_g97 , ifLocalVar25_g97 , saturate( lerpResult5_g99 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch84 = lerpResult4_g97;
				#else
				float4 staticSwitch84 = tex2D( _Layer03RAOHMTexture, temp_output_107_0 );
				#endif
				float temp_output_79_0 = ( 1.0 - (staticSwitch84).b );
				float4 lerpResult61 = lerp( _Layer03AColor , _Layer03BColor , temp_output_79_0);
				float temp_output_1_0_g290 = _Blend02Contrast;
				float lerpResult16_g245 = lerp( temp_output_3_0_g245 , temp_output_2_0_g245 , temp_output_24_0_g245);
				float temp_output_2_0_g289 = lerpResult16_g245;
				float temp_output_1_0_g289 = pow( ( 1.0 - saturate( break196.g ) ) , _Blend02Bias );
				float temp_output_3_0_g289 = temp_output_79_0;
				float temp_output_17_0_g289 = saturate( ( ( ( temp_output_2_0_g289 + ( 1.0 - temp_output_1_0_g289 ) ) + 1.0 ) - ( ( temp_output_1_0_g289 * 2.0 ) + temp_output_3_0_g289 ) ) );
				float lerpResult5_g290 = lerp( ( 0.0 - temp_output_1_0_g290 ) , ( temp_output_1_0_g290 + 1.0 ) , temp_output_17_0_g289);
				float temp_output_24_0_g289 = saturate( lerpResult5_g290 );
				float temp_output_51_0 = temp_output_24_0_g289;
				float4 lerpResult57 = lerp( lerpResult56 , lerpResult61 , temp_output_51_0);
				float2 lerpResult117 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_108_0 = ( lerpResult117 * _Layer04Tiling );
				float temp_output_23_0_g100 = 0.0;
				float3 temp_cast_13 = (( _Layer04Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g100 = ( WorldPosition / ( abs( temp_cast_13 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xz );
				float4 ifLocalVar24_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar24_g100 = float4( (tex2DNode31_g100).rgb , 0.0 );
				else
				ifLocalVar24_g100 = tex2DNode31_g100;
				float4 tex2DNode29_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).yz );
				float4 ifLocalVar21_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar21_g100 = float4( (tex2DNode29_g100).rgb , 0.0 );
				else
				ifLocalVar21_g100 = tex2DNode29_g100;
				float temp_output_9_0_g100 = 0.5;
				float temp_output_1_0_g101 = temp_output_9_0_g100;
				float3 temp_output_15_0_g100 = normalizedWorldNormal;
				float lerpResult5_g101 = lerp( ( 0.0 - temp_output_1_0_g101 ) , ( temp_output_1_0_g101 + 1.0 ) , abs( (temp_output_15_0_g100).x ));
				float4 lerpResult3_g100 = lerp( ifLocalVar24_g100 , ifLocalVar21_g100 , saturate( lerpResult5_g101 ));
				float4 tex2DNode32_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xy );
				float4 ifLocalVar25_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar25_g100 = float4( (tex2DNode32_g100).rgb , 0.0 );
				else
				ifLocalVar25_g100 = tex2DNode32_g100;
				float temp_output_1_0_g102 = temp_output_9_0_g100;
				float lerpResult5_g102 = lerp( ( 0.0 - temp_output_1_0_g102 ) , ( temp_output_1_0_g102 + 1.0 ) , abs( (temp_output_15_0_g100).z ));
				float4 lerpResult4_g100 = lerp( lerpResult3_g100 , ifLocalVar25_g100 , saturate( lerpResult5_g102 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch85 = lerpResult4_g100;
				#else
				float4 staticSwitch85 = tex2D( _Layer04RAOHMTexture, temp_output_108_0 );
				#endif
				float temp_output_81_0 = ( 1.0 - (staticSwitch85).b );
				float4 lerpResult62 = lerp( _Layer04AColor , _Layer04BColor , temp_output_81_0);
				float temp_output_1_0_g292 = _Blend03Contrast;
				float lerpResult16_g289 = lerp( temp_output_3_0_g289 , temp_output_2_0_g289 , temp_output_24_0_g289);
				float temp_output_2_0_g291 = lerpResult16_g289;
				float temp_output_1_0_g291 = pow( ( 1.0 - saturate( break196.b ) ) , _Blend03Bias );
				float temp_output_3_0_g291 = temp_output_81_0;
				float temp_output_17_0_g291 = saturate( ( ( ( temp_output_2_0_g291 + ( 1.0 - temp_output_1_0_g291 ) ) + 1.0 ) - ( ( temp_output_1_0_g291 * 2.0 ) + temp_output_3_0_g291 ) ) );
				float lerpResult5_g292 = lerp( ( 0.0 - temp_output_1_0_g292 ) , ( temp_output_1_0_g292 + 1.0 ) , temp_output_17_0_g291);
				float temp_output_24_0_g291 = saturate( lerpResult5_g292 );
				float temp_output_52_0 = temp_output_24_0_g291;
				float4 lerpResult58 = lerp( lerpResult57 , lerpResult62 , temp_output_52_0);
				float temp_output_1_0_g311 = _DistanceBlendContrast;
				float lerpResult5_g311 = lerp( ( 0.0 - temp_output_1_0_g311 ) , ( temp_output_1_0_g311 + 1.0 ) , ( ( 20.0 + 0.0 ) / 2.0 ));
				float temp_output_26_0 = saturate( lerpResult5_g311 );
				float4 lerpResult17 = lerp( lerpResult58 , lerpResult62 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float4 staticSwitch15 = lerpResult17;
				#else
				float4 staticSwitch15 = lerpResult58;
				#endif
				
				
				float3 Albedo = staticSwitch15.rgb;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				half4 color = half4( Albedo, Alpha );

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				return color;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormals" }

			ZWrite On
			Blend One Zero
            ZTest LEqual
            ZWrite On

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float3 worldNormal : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Layer01AColor;
			float4 _Layer04BColor;
			float4 _Layer04AColor;
			float4 _Layer03AColor;
			float4 _Layer02BColor;
			float4 _Layer02AColor;
			float4 _Layer03BColor;
			float4 _Layer01BColor;
			float _FlowBlend;
			float _Layer4Spec;
			float _Layer3Spec;
			float _Layer2Spec;
			float _Layer1Spec;
			float _Layer04NormalFlatten;
			float _Layer03NormalFlatten;
			float _Layer02NormalFlatten;
			float _Layer01NormalFlatten;
			float _DistanceBlendContrast;
			float _Blend03Bias;
			float _Blend03Contrast;
			float _Layer04Tiling;
			float _FlowStrength;
			float _UVWorldXYScale;
			float _Blend02Contrast;
			float _Layer03Tiling;
			float _RoughnessMin;
			float _DISTANCE_FIELD_NOT_SUPPORTED;
			float _Blend01Bias;
			float _BlendsDivisor;
			float _BlendsOffset;
			float _Blend01Contrast;
			float _Layer02Tiling;
			float _DistanceFieldUVScale;
			float _FlowUVScale;
			float _Layer01Tiling;
			float _Layer01FlowIntensity;
			float _Blend02Bias;
			float _RoughnessMax;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 normalWS = TransformObjectToWorldNormal( v.ase_normal );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.worldNormal = normalWS;

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif
				
				return float4(PackNormalOctRectEncode(TransformWorldToViewDir(IN.worldNormal, true)), 0.0, 0.0);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "GBuffer"
			Tags { "LightMode"="UniversalGBuffer" }
			
			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			#pragma multi_compile _ _GBUFFER_NORMALS_OCT
			
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_GBUFFER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/UnityGBuffer.hlsl"

			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_TANGENT
			#define ASE_NEEDS_FRAG_WORLD_BITANGENT
			#pragma shader_feature_local _BLENDON_ON
			#pragma shader_feature_local _SWITCHTOWORLDTRIPLANAR_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_color : COLOR;
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Layer01AColor;
			float4 _Layer04BColor;
			float4 _Layer04AColor;
			float4 _Layer03AColor;
			float4 _Layer02BColor;
			float4 _Layer02AColor;
			float4 _Layer03BColor;
			float4 _Layer01BColor;
			float _FlowBlend;
			float _Layer4Spec;
			float _Layer3Spec;
			float _Layer2Spec;
			float _Layer1Spec;
			float _Layer04NormalFlatten;
			float _Layer03NormalFlatten;
			float _Layer02NormalFlatten;
			float _Layer01NormalFlatten;
			float _DistanceBlendContrast;
			float _Blend03Bias;
			float _Blend03Contrast;
			float _Layer04Tiling;
			float _FlowStrength;
			float _UVWorldXYScale;
			float _Blend02Contrast;
			float _Layer03Tiling;
			float _RoughnessMin;
			float _DISTANCE_FIELD_NOT_SUPPORTED;
			float _Blend01Bias;
			float _BlendsDivisor;
			float _BlendsOffset;
			float _Blend01Contrast;
			float _Layer02Tiling;
			float _DistanceFieldUVScale;
			float _FlowUVScale;
			float _Layer01Tiling;
			float _Layer01FlowIntensity;
			float _Blend02Bias;
			float _RoughnessMax;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _Layer01RAOHMTexture;
			sampler2D _Texture3;
			sampler2D _Layer02RAOHMTexture;
			sampler2D _Layer03RAOHMTexture;
			sampler2D _Layer04RAOHMTexture;
			sampler2D _Layer01Normal;
			sampler2D _Layer02Normal;
			sampler2D _Layer03Normal;
			sampler2D _Layer04Normal;
			sampler2D _TerrainNormal;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_color = v.ase_color;
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			FragmentOutput frag ( VertexOutput IN 
								#ifdef ASE_DEPTH_WRITE_ON
								,out float outputDepth : ASE_SV_DEPTH
								#endif
								 )
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float2 temp_output_131_0 = (( WorldPosition / ( _UVWorldXYScale * 512.0 ) )).xy;
				float2 temp_output_21_0_g83 = temp_output_131_0;
				float temp_output_11_0_g83 = 0.0;
				float3 temp_cast_0 = (( _DISTANCE_FIELD_NOT_SUPPORTED / _DistanceFieldUVScale )).xxx;
				float3 worldToObjDir143 = mul( GetWorldToObjectMatrix(), float4( temp_cast_0, 0 ) ).xyz;
				float2 normalizeResult138 = normalize( ( ( (worldToObjDir143).xy + ( (tex2D( _Texture3, (( WorldPosition / ( _FlowUVScale * 512.0 ) )).xy )).rg / float2( 32,0 ) ) ) - float2( 0.5,0 ) ) );
				float2 lerpResult134 = lerp( temp_output_131_0 , ( normalizeResult138 * _FlowStrength ) , _FlowBlend);
				float2 temp_output_15_0_g83 = ( lerpResult134 * float2( -1,1 ) );
				float2 temp_output_16_0_g83 = ( frac( ( temp_output_11_0_g83 - 0.5 ) ) * temp_output_15_0_g83 );
				float2 temp_output_19_0_g83 = ( temp_output_21_0_g83 + temp_output_16_0_g83 );
				float temp_output_12_0_g83 = frac( temp_output_11_0_g83 );
				float2 temp_output_13_0_g83 = ( temp_output_15_0_g83 * temp_output_12_0_g83 );
				float2 temp_output_22_0_g83 = ( temp_output_13_0_g83 + temp_output_21_0_g83 );
				float temp_output_2_0_g85 = temp_output_12_0_g83;
				float temp_output_3_0_g85 = 1.0;
				float temp_output_4_0_g85 = ( temp_output_2_0_g85 + ( 0.25 * temp_output_3_0_g85 ) );
				float ifLocalVar24_g85 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g85 = temp_output_2_0_g85;
				else
				ifLocalVar24_g85 = temp_output_4_0_g85;
				float temp_output_7_0_g85 = frac( ( ifLocalVar24_g85 / temp_output_3_0_g85 ) );
				float temp_output_8_0_g85 = ( 2.0 * temp_output_7_0_g85 );
				float temp_output_12_0_g85 = floor( temp_output_8_0_g85 );
				float lerpResult13_g85 = lerp( temp_output_8_0_g85 , ( 2.0 * ( 1.0 - temp_output_7_0_g85 ) ) , temp_output_12_0_g85);
				float temp_output_78_0_g83 = lerpResult13_g85;
				float2 lerpResult60_g83 = lerp( temp_output_19_0_g83 , temp_output_22_0_g83 , temp_output_78_0_g83);
				float2 temp_output_124_62 = lerpResult60_g83;
				float2 lerpResult114 = lerp( temp_output_131_0 , temp_output_124_62 , _Layer01FlowIntensity);
				float2 temp_output_105_0 = ( lerpResult114 * _Layer01Tiling );
				float temp_output_23_0_g94 = 0.0;
				float3 temp_cast_1 = (( _Layer01Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g94 = ( WorldPosition / ( abs( temp_cast_1 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xz );
				float4 ifLocalVar24_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar24_g94 = float4( (tex2DNode31_g94).rgb , 0.0 );
				else
				ifLocalVar24_g94 = tex2DNode31_g94;
				float4 tex2DNode29_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).yz );
				float4 ifLocalVar21_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar21_g94 = float4( (tex2DNode29_g94).rgb , 0.0 );
				else
				ifLocalVar21_g94 = tex2DNode29_g94;
				float temp_output_9_0_g94 = 0.5;
				float temp_output_1_0_g95 = temp_output_9_0_g94;
				float3 normalizedWorldNormal = normalize( WorldNormal );
				float3 temp_output_15_0_g94 = normalizedWorldNormal;
				float lerpResult5_g95 = lerp( ( 0.0 - temp_output_1_0_g95 ) , ( temp_output_1_0_g95 + 1.0 ) , abs( (temp_output_15_0_g94).x ));
				float4 lerpResult3_g94 = lerp( ifLocalVar24_g94 , ifLocalVar21_g94 , saturate( lerpResult5_g95 ));
				float4 tex2DNode32_g94 = tex2D( _Layer01RAOHMTexture, (temp_output_41_0_g94).xy );
				float4 ifLocalVar25_g94 = 0;
				if( temp_output_23_0_g94 == 0.0 )
				ifLocalVar25_g94 = float4( (tex2DNode32_g94).rgb , 0.0 );
				else
				ifLocalVar25_g94 = tex2DNode32_g94;
				float temp_output_1_0_g96 = temp_output_9_0_g94;
				float lerpResult5_g96 = lerp( ( 0.0 - temp_output_1_0_g96 ) , ( temp_output_1_0_g96 + 1.0 ) , abs( (temp_output_15_0_g94).z ));
				float4 lerpResult4_g94 = lerp( lerpResult3_g94 , ifLocalVar25_g94 , saturate( lerpResult5_g96 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch82 = lerpResult4_g94;
				#else
				float4 staticSwitch82 = tex2D( _Layer01RAOHMTexture, temp_output_105_0 );
				#endif
				float temp_output_74_0 = ( 1.0 - (staticSwitch82).b );
				float4 lerpResult59 = lerp( _Layer01AColor , _Layer01BColor , temp_output_74_0);
				float2 lerpResult115 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_106_0 = ( lerpResult115 * _Layer02Tiling );
				float temp_output_23_0_g91 = 0.0;
				float3 temp_cast_5 = (( _Layer02Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g91 = ( WorldPosition / ( abs( temp_cast_5 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xz );
				float4 ifLocalVar24_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar24_g91 = float4( (tex2DNode31_g91).rgb , 0.0 );
				else
				ifLocalVar24_g91 = tex2DNode31_g91;
				float4 tex2DNode29_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).yz );
				float4 ifLocalVar21_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar21_g91 = float4( (tex2DNode29_g91).rgb , 0.0 );
				else
				ifLocalVar21_g91 = tex2DNode29_g91;
				float temp_output_9_0_g91 = 0.5;
				float temp_output_1_0_g92 = temp_output_9_0_g91;
				float3 temp_output_15_0_g91 = normalizedWorldNormal;
				float lerpResult5_g92 = lerp( ( 0.0 - temp_output_1_0_g92 ) , ( temp_output_1_0_g92 + 1.0 ) , abs( (temp_output_15_0_g91).x ));
				float4 lerpResult3_g91 = lerp( ifLocalVar24_g91 , ifLocalVar21_g91 , saturate( lerpResult5_g92 ));
				float4 tex2DNode32_g91 = tex2D( _Layer02RAOHMTexture, (temp_output_41_0_g91).xy );
				float4 ifLocalVar25_g91 = 0;
				if( temp_output_23_0_g91 == 0.0 )
				ifLocalVar25_g91 = float4( (tex2DNode32_g91).rgb , 0.0 );
				else
				ifLocalVar25_g91 = tex2DNode32_g91;
				float temp_output_1_0_g93 = temp_output_9_0_g91;
				float lerpResult5_g93 = lerp( ( 0.0 - temp_output_1_0_g93 ) , ( temp_output_1_0_g93 + 1.0 ) , abs( (temp_output_15_0_g91).z ));
				float4 lerpResult4_g91 = lerp( lerpResult3_g91 , ifLocalVar25_g91 , saturate( lerpResult5_g93 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch83 = lerpResult4_g91;
				#else
				float4 staticSwitch83 = tex2D( _Layer02RAOHMTexture, temp_output_106_0 );
				#endif
				float temp_output_77_0 = ( 1.0 - (staticSwitch83).b );
				float4 lerpResult60 = lerp( _Layer02AColor , _Layer02BColor , temp_output_77_0);
				float temp_output_1_0_g246 = _Blend01Contrast;
				float temp_output_2_0_g245 = temp_output_74_0;
				float4 break196 = ( ( IN.ase_color + _BlendsOffset ) / _BlendsDivisor );
				float temp_output_1_0_g245 = pow( ( 1.0 - saturate( break196.r ) ) , _Blend01Bias );
				float temp_output_3_0_g245 = temp_output_77_0;
				float temp_output_17_0_g245 = saturate( ( ( ( temp_output_2_0_g245 + ( 1.0 - temp_output_1_0_g245 ) ) + 1.0 ) - ( ( temp_output_1_0_g245 * 2.0 ) + temp_output_3_0_g245 ) ) );
				float lerpResult5_g246 = lerp( ( 0.0 - temp_output_1_0_g246 ) , ( temp_output_1_0_g246 + 1.0 ) , temp_output_17_0_g245);
				float temp_output_24_0_g245 = saturate( lerpResult5_g246 );
				float temp_output_50_0 = temp_output_24_0_g245;
				float4 lerpResult56 = lerp( lerpResult59 , lerpResult60 , temp_output_50_0);
				float2 lerpResult116 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_107_0 = ( lerpResult116 * _Layer03Tiling );
				float temp_output_23_0_g97 = 0.0;
				float3 temp_cast_9 = (( _Layer03Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g97 = ( WorldPosition / ( abs( temp_cast_9 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xz );
				float4 ifLocalVar24_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar24_g97 = float4( (tex2DNode31_g97).rgb , 0.0 );
				else
				ifLocalVar24_g97 = tex2DNode31_g97;
				float4 tex2DNode29_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).yz );
				float4 ifLocalVar21_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar21_g97 = float4( (tex2DNode29_g97).rgb , 0.0 );
				else
				ifLocalVar21_g97 = tex2DNode29_g97;
				float temp_output_9_0_g97 = 0.5;
				float temp_output_1_0_g98 = temp_output_9_0_g97;
				float3 temp_output_15_0_g97 = normalizedWorldNormal;
				float lerpResult5_g98 = lerp( ( 0.0 - temp_output_1_0_g98 ) , ( temp_output_1_0_g98 + 1.0 ) , abs( (temp_output_15_0_g97).x ));
				float4 lerpResult3_g97 = lerp( ifLocalVar24_g97 , ifLocalVar21_g97 , saturate( lerpResult5_g98 ));
				float4 tex2DNode32_g97 = tex2D( _Layer03RAOHMTexture, (temp_output_41_0_g97).xy );
				float4 ifLocalVar25_g97 = 0;
				if( temp_output_23_0_g97 == 0.0 )
				ifLocalVar25_g97 = float4( (tex2DNode32_g97).rgb , 0.0 );
				else
				ifLocalVar25_g97 = tex2DNode32_g97;
				float temp_output_1_0_g99 = temp_output_9_0_g97;
				float lerpResult5_g99 = lerp( ( 0.0 - temp_output_1_0_g99 ) , ( temp_output_1_0_g99 + 1.0 ) , abs( (temp_output_15_0_g97).z ));
				float4 lerpResult4_g97 = lerp( lerpResult3_g97 , ifLocalVar25_g97 , saturate( lerpResult5_g99 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch84 = lerpResult4_g97;
				#else
				float4 staticSwitch84 = tex2D( _Layer03RAOHMTexture, temp_output_107_0 );
				#endif
				float temp_output_79_0 = ( 1.0 - (staticSwitch84).b );
				float4 lerpResult61 = lerp( _Layer03AColor , _Layer03BColor , temp_output_79_0);
				float temp_output_1_0_g290 = _Blend02Contrast;
				float lerpResult16_g245 = lerp( temp_output_3_0_g245 , temp_output_2_0_g245 , temp_output_24_0_g245);
				float temp_output_2_0_g289 = lerpResult16_g245;
				float temp_output_1_0_g289 = pow( ( 1.0 - saturate( break196.g ) ) , _Blend02Bias );
				float temp_output_3_0_g289 = temp_output_79_0;
				float temp_output_17_0_g289 = saturate( ( ( ( temp_output_2_0_g289 + ( 1.0 - temp_output_1_0_g289 ) ) + 1.0 ) - ( ( temp_output_1_0_g289 * 2.0 ) + temp_output_3_0_g289 ) ) );
				float lerpResult5_g290 = lerp( ( 0.0 - temp_output_1_0_g290 ) , ( temp_output_1_0_g290 + 1.0 ) , temp_output_17_0_g289);
				float temp_output_24_0_g289 = saturate( lerpResult5_g290 );
				float temp_output_51_0 = temp_output_24_0_g289;
				float4 lerpResult57 = lerp( lerpResult56 , lerpResult61 , temp_output_51_0);
				float2 lerpResult117 = lerp( temp_output_131_0 , temp_output_124_62 , 0.0);
				float2 temp_output_108_0 = ( lerpResult117 * _Layer04Tiling );
				float temp_output_23_0_g100 = 0.0;
				float3 temp_cast_13 = (( _Layer04Tiling * 5.12 )).xxx;
				float3 temp_output_41_0_g100 = ( WorldPosition / ( abs( temp_cast_13 ) * float3( -1,-1,-1 ) ) );
				float4 tex2DNode31_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xz );
				float4 ifLocalVar24_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar24_g100 = float4( (tex2DNode31_g100).rgb , 0.0 );
				else
				ifLocalVar24_g100 = tex2DNode31_g100;
				float4 tex2DNode29_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).yz );
				float4 ifLocalVar21_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar21_g100 = float4( (tex2DNode29_g100).rgb , 0.0 );
				else
				ifLocalVar21_g100 = tex2DNode29_g100;
				float temp_output_9_0_g100 = 0.5;
				float temp_output_1_0_g101 = temp_output_9_0_g100;
				float3 temp_output_15_0_g100 = normalizedWorldNormal;
				float lerpResult5_g101 = lerp( ( 0.0 - temp_output_1_0_g101 ) , ( temp_output_1_0_g101 + 1.0 ) , abs( (temp_output_15_0_g100).x ));
				float4 lerpResult3_g100 = lerp( ifLocalVar24_g100 , ifLocalVar21_g100 , saturate( lerpResult5_g101 ));
				float4 tex2DNode32_g100 = tex2D( _Layer04RAOHMTexture, (temp_output_41_0_g100).xy );
				float4 ifLocalVar25_g100 = 0;
				if( temp_output_23_0_g100 == 0.0 )
				ifLocalVar25_g100 = float4( (tex2DNode32_g100).rgb , 0.0 );
				else
				ifLocalVar25_g100 = tex2DNode32_g100;
				float temp_output_1_0_g102 = temp_output_9_0_g100;
				float lerpResult5_g102 = lerp( ( 0.0 - temp_output_1_0_g102 ) , ( temp_output_1_0_g102 + 1.0 ) , abs( (temp_output_15_0_g100).z ));
				float4 lerpResult4_g100 = lerp( lerpResult3_g100 , ifLocalVar25_g100 , saturate( lerpResult5_g102 ));
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float4 staticSwitch85 = lerpResult4_g100;
				#else
				float4 staticSwitch85 = tex2D( _Layer04RAOHMTexture, temp_output_108_0 );
				#endif
				float temp_output_81_0 = ( 1.0 - (staticSwitch85).b );
				float4 lerpResult62 = lerp( _Layer04AColor , _Layer04BColor , temp_output_81_0);
				float temp_output_1_0_g292 = _Blend03Contrast;
				float lerpResult16_g289 = lerp( temp_output_3_0_g289 , temp_output_2_0_g289 , temp_output_24_0_g289);
				float temp_output_2_0_g291 = lerpResult16_g289;
				float temp_output_1_0_g291 = pow( ( 1.0 - saturate( break196.b ) ) , _Blend03Bias );
				float temp_output_3_0_g291 = temp_output_81_0;
				float temp_output_17_0_g291 = saturate( ( ( ( temp_output_2_0_g291 + ( 1.0 - temp_output_1_0_g291 ) ) + 1.0 ) - ( ( temp_output_1_0_g291 * 2.0 ) + temp_output_3_0_g291 ) ) );
				float lerpResult5_g292 = lerp( ( 0.0 - temp_output_1_0_g292 ) , ( temp_output_1_0_g292 + 1.0 ) , temp_output_17_0_g291);
				float temp_output_24_0_g291 = saturate( lerpResult5_g292 );
				float temp_output_52_0 = temp_output_24_0_g291;
				float4 lerpResult58 = lerp( lerpResult57 , lerpResult62 , temp_output_52_0);
				float temp_output_1_0_g311 = _DistanceBlendContrast;
				float lerpResult5_g311 = lerp( ( 0.0 - temp_output_1_0_g311 ) , ( temp_output_1_0_g311 + 1.0 ) , ( ( 20.0 + 0.0 ) / 2.0 ));
				float temp_output_26_0 = saturate( lerpResult5_g311 );
				float4 lerpResult17 = lerp( lerpResult58 , lerpResult62 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float4 staticSwitch15 = lerpResult17;
				#else
				float4 staticSwitch15 = lerpResult58;
				#endif
				
				int temp_output_76_0_g247 = 0;
				int temp_output_75_0_g247 = (int)1.0;
				float3 temp_output_72_0_g247 = WorldPosition;
				float temp_output_69_0_g247 = ( abs( ( _Layer01Tiling * 5.12 ) ) * -1.0 );
				float3 temp_output_68_0_g247 = ( temp_output_72_0_g247 / temp_output_69_0_g247 );
				float4 tex2DNode27_g247 = tex2D( _Layer01Normal, (temp_output_68_0_g247).xz );
				float3x3 ase_tangentToWorldFast = float3x3(WorldTangent.x,WorldBiTangent.x,WorldNormal.x,WorldTangent.y,WorldBiTangent.y,WorldNormal.y,WorldTangent.z,WorldBiTangent.z,WorldNormal.z);
				float3 tangentToWorldDir63_g247 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g247 = ( tangentToWorldDir63_g247 * float3( 1,0,0 ) );
				float3 appendResult35_g247 = (float3(( tex2DNode27_g247.r * -1.0 ) , ( tex2DNode27_g247.b * (temp_output_54_0_g247).y ) , ( tex2DNode27_g247.g * -1.0 )));
				float4 tex2DNode28_g247 = tex2D( _Layer01Normal, (temp_output_68_0_g247).yz );
				float temp_output_51_0_g247 = (temp_output_54_0_g247).x;
				float3 appendResult36_g247 = (float3(( tex2DNode28_g247.r * -1.0 ) , ( tex2DNode28_g247.b * temp_output_51_0_g247 ) , ( tex2DNode28_g247.g * -1.0 )));
				float temp_output_1_0_g248 = 0.0;
				float lerpResult5_g248 = lerp( ( 0.0 - temp_output_1_0_g248 ) , ( temp_output_1_0_g248 + 1.0 ) , abs( temp_output_51_0_g247 ));
				float temp_output_58_0_g247 = saturate( lerpResult5_g248 );
				float3 lerpResult24_g247 = lerp( appendResult35_g247 , appendResult36_g247 , temp_output_58_0_g247);
				float4 tex2DNode29_g247 = tex2D( _Layer01Normal, (temp_output_68_0_g247).xy );
				float temp_output_53_0_g247 = (temp_output_54_0_g247).z;
				float3 appendResult50_g247 = (float3(( (tex2DNode29_g247).rg * float2( -1,-1 ) ) , ( tex2DNode29_g247.b * temp_output_53_0_g247 )));
				float temp_output_1_0_g260 = 0.5;
				float lerpResult5_g260 = lerp( ( 0.0 - temp_output_1_0_g260 ) , ( temp_output_1_0_g260 + 1.0 ) , abs( temp_output_53_0_g247 ));
				float temp_output_59_0_g247 = saturate( lerpResult5_g260 );
				float3 lerpResult25_g247 = lerp( lerpResult24_g247 , appendResult50_g247 , temp_output_59_0_g247);
				float3 normalizeResult20_g247 = normalize( lerpResult25_g247 );
				float3 temp_output_22_0_g249 = WorldNormal;
				float3 normalizeResult6_g252 = normalize( temp_output_22_0_g249 );
				float3 normalizeResult7_g252 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g252 = cross( normalizeResult6_g252 , normalizeResult7_g252 );
				float3 temp_output_10_0_g253 = temp_output_5_0_g252;
				float dotResult5_g253 = dot( temp_output_10_0_g253 , temp_output_10_0_g253 );
				float3 normalizeResult7_g253 = normalize( temp_output_10_0_g253 );
				float2 appendResult8_g253 = (float2(normalizeResult7_g253.x , 0.0));
				float2 appendResult11_g253 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g253 = 0;
				if( dotResult5_g253 <= 1E-06 )
				ifLocalVar4_g253 = appendResult11_g253;
				else
				ifLocalVar4_g253 = appendResult8_g253;
				float3 temp_cast_24 = (temp_output_69_0_g247).xxx;
				float3 temp_output_59_0_g249 = ( temp_output_72_0_g247 / temp_cast_24 );
				float dotResult49_g249 = dot( temp_output_22_0_g249 , float3(0,1,0) );
				float ifLocalVar45_g249 = 0;
				if( dotResult49_g249 > 0.0 )
				ifLocalVar45_g249 = -1.0;
				else if( dotResult49_g249 < 0.0 )
				ifLocalVar45_g249 = 1.0;
				float3 appendResult38_g249 = (float3(ifLocalVar45_g249 , -1.0 , 1.0));
				float dotResult54_g249 = dot( temp_output_22_0_g249 , float3(1,0,0) );
				float ifLocalVar50_g249 = 0;
				if( dotResult54_g249 > 0.0 )
				ifLocalVar50_g249 = 1.0;
				else if( dotResult54_g249 < 0.0 )
				ifLocalVar50_g249 = -1.0;
				float3 appendResult37_g249 = (float3(ifLocalVar50_g249 , -1.0 , 1.0));
				float4 lerpResult27_g249 = lerp( ( tex2D( _Layer01Normal, (temp_output_59_0_g249).xz ) * float4( appendResult38_g249 , 0.0 ) ) , ( tex2D( _Layer01Normal, (temp_output_59_0_g249).yz ) * float4( appendResult37_g249 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g247 ));
				float3 break5_g256 = lerpResult27_g249.rgb;
				float3 temp_output_10_0_g254 = cross( temp_output_5_0_g252 , normalizeResult6_g252 );
				float dotResult5_g254 = dot( temp_output_10_0_g254 , temp_output_10_0_g254 );
				float3 normalizeResult7_g254 = normalize( temp_output_10_0_g254 );
				float2 appendResult8_g254 = (float2(normalizeResult7_g254.x , 0.0));
				float2 appendResult11_g254 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g254 = 0;
				if( dotResult5_g254 <= 1E-06 )
				ifLocalVar4_g254 = appendResult11_g254;
				else
				ifLocalVar4_g254 = appendResult8_g254;
				float3 temp_output_25_0_g249 = ( ( float3( (ifLocalVar4_g253).xy ,  0.0 ) * break5_g256.x ) + ( float3( (ifLocalVar4_g254).xy ,  0.0 ) * break5_g256.y ) + ( normalizeResult6_g252 * break5_g256.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g257 = normalize( temp_output_22_0_g249 );
				float3 normalizeResult7_g257 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g257 = cross( normalizeResult6_g257 , normalizeResult7_g257 );
				float3 temp_output_10_0_g258 = temp_output_5_0_g257;
				float dotResult5_g258 = dot( temp_output_10_0_g258 , temp_output_10_0_g258 );
				float3 normalizeResult7_g258 = normalize( temp_output_10_0_g258 );
				float2 appendResult8_g258 = (float2(normalizeResult7_g258.x , 0.0));
				float2 appendResult11_g258 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g258 = 0;
				if( dotResult5_g258 <= 1E-06 )
				ifLocalVar4_g258 = appendResult11_g258;
				else
				ifLocalVar4_g258 = appendResult8_g258;
				float dotResult20_g249 = dot( temp_output_22_0_g249 , float3(0,0,1) );
				float ifLocalVar16_g249 = 0;
				if( dotResult20_g249 > 0.0 )
				ifLocalVar16_g249 = 1.0;
				else if( dotResult20_g249 < 0.0 )
				ifLocalVar16_g249 = -1.0;
				float3 appendResult14_g249 = (float3(ifLocalVar16_g249 , -1.0 , 1.0));
				float3 break5_g250 = ( tex2D( _Layer01Normal, (temp_output_59_0_g249).xy ) * float4( appendResult14_g249 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g259 = cross( temp_output_5_0_g257 , normalizeResult6_g257 );
				float dotResult5_g259 = dot( temp_output_10_0_g259 , temp_output_10_0_g259 );
				float3 normalizeResult7_g259 = normalize( temp_output_10_0_g259 );
				float2 appendResult8_g259 = (float2(normalizeResult7_g259.x , 0.0));
				float2 appendResult11_g259 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g259 = 0;
				if( dotResult5_g259 <= 1E-06 )
				ifLocalVar4_g259 = appendResult11_g259;
				else
				ifLocalVar4_g259 = appendResult8_g259;
				float3 temp_output_9_0_g249 = ( ( float3( (ifLocalVar4_g258).xy ,  0.0 ) * break5_g250.x ) + ( float3( (ifLocalVar4_g259).xy ,  0.0 ) * break5_g250.y ) + ( normalizeResult6_g257 * break5_g250.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g249 = ( 1.0 - temp_output_59_0_g247 );
				float3 lerpResult5_g249 = lerp( temp_output_25_0_g249 , temp_output_9_0_g249 , temp_output_33_0_g249);
				float3 temp_output_18_0_g247 = ( (float)temp_output_75_0_g247 == 0.0 ? normalizeResult20_g247 : lerpResult5_g249 );
				float3x3 ase_worldToTangent = float3x3(WorldTangent,WorldBiTangent,WorldNormal);
				float3 worldToTangentDir11_g247 = mul( ase_worldToTangent, temp_output_18_0_g247);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch91 = ( (float)temp_output_76_0_g247 == 0.0 ? worldToTangentDir11_g247 : temp_output_18_0_g247 );
				#else
				float3 staticSwitch91 = UnpackNormalScale( tex2D( _Layer01Normal, temp_output_105_0 ), 1.0f );
				#endif
				float3 lerpResult1_g316 = lerp( staticSwitch91 , float3(0,0,1) , _Layer01NormalFlatten);
				int temp_output_76_0_g261 = 0;
				int temp_output_75_0_g261 = (int)1.0;
				float3 temp_output_72_0_g261 = WorldPosition;
				float temp_output_69_0_g261 = ( abs( ( _Layer02Tiling * 5.12 ) ) * -1.0 );
				float3 temp_output_68_0_g261 = ( temp_output_72_0_g261 / temp_output_69_0_g261 );
				float4 tex2DNode27_g261 = tex2D( _Layer02Normal, (temp_output_68_0_g261).xz );
				float3 tangentToWorldDir63_g261 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g261 = ( tangentToWorldDir63_g261 * float3( 1,0,0 ) );
				float3 appendResult35_g261 = (float3(( tex2DNode27_g261.r * -1.0 ) , ( tex2DNode27_g261.b * (temp_output_54_0_g261).y ) , ( tex2DNode27_g261.g * -1.0 )));
				float4 tex2DNode28_g261 = tex2D( _Layer02Normal, (temp_output_68_0_g261).yz );
				float temp_output_51_0_g261 = (temp_output_54_0_g261).x;
				float3 appendResult36_g261 = (float3(( tex2DNode28_g261.r * -1.0 ) , ( tex2DNode28_g261.b * temp_output_51_0_g261 ) , ( tex2DNode28_g261.g * -1.0 )));
				float temp_output_1_0_g262 = 0.0;
				float lerpResult5_g262 = lerp( ( 0.0 - temp_output_1_0_g262 ) , ( temp_output_1_0_g262 + 1.0 ) , abs( temp_output_51_0_g261 ));
				float temp_output_58_0_g261 = saturate( lerpResult5_g262 );
				float3 lerpResult24_g261 = lerp( appendResult35_g261 , appendResult36_g261 , temp_output_58_0_g261);
				float4 tex2DNode29_g261 = tex2D( _Layer02Normal, (temp_output_68_0_g261).xy );
				float temp_output_53_0_g261 = (temp_output_54_0_g261).z;
				float3 appendResult50_g261 = (float3(( (tex2DNode29_g261).rg * float2( -1,-1 ) ) , ( tex2DNode29_g261.b * temp_output_53_0_g261 )));
				float temp_output_1_0_g274 = 0.5;
				float lerpResult5_g274 = lerp( ( 0.0 - temp_output_1_0_g274 ) , ( temp_output_1_0_g274 + 1.0 ) , abs( temp_output_53_0_g261 ));
				float temp_output_59_0_g261 = saturate( lerpResult5_g274 );
				float3 lerpResult25_g261 = lerp( lerpResult24_g261 , appendResult50_g261 , temp_output_59_0_g261);
				float3 normalizeResult20_g261 = normalize( lerpResult25_g261 );
				float3 temp_output_22_0_g263 = WorldNormal;
				float3 normalizeResult6_g266 = normalize( temp_output_22_0_g263 );
				float3 normalizeResult7_g266 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g266 = cross( normalizeResult6_g266 , normalizeResult7_g266 );
				float3 temp_output_10_0_g267 = temp_output_5_0_g266;
				float dotResult5_g267 = dot( temp_output_10_0_g267 , temp_output_10_0_g267 );
				float3 normalizeResult7_g267 = normalize( temp_output_10_0_g267 );
				float2 appendResult8_g267 = (float2(normalizeResult7_g267.x , 0.0));
				float2 appendResult11_g267 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g267 = 0;
				if( dotResult5_g267 <= 1E-06 )
				ifLocalVar4_g267 = appendResult11_g267;
				else
				ifLocalVar4_g267 = appendResult8_g267;
				float3 temp_cast_46 = (temp_output_69_0_g261).xxx;
				float3 temp_output_59_0_g263 = ( temp_output_72_0_g261 / temp_cast_46 );
				float dotResult49_g263 = dot( temp_output_22_0_g263 , float3(0,1,0) );
				float ifLocalVar45_g263 = 0;
				if( dotResult49_g263 > 0.0 )
				ifLocalVar45_g263 = -1.0;
				else if( dotResult49_g263 < 0.0 )
				ifLocalVar45_g263 = 1.0;
				float3 appendResult38_g263 = (float3(ifLocalVar45_g263 , -1.0 , 1.0));
				float dotResult54_g263 = dot( temp_output_22_0_g263 , float3(1,0,0) );
				float ifLocalVar50_g263 = 0;
				if( dotResult54_g263 > 0.0 )
				ifLocalVar50_g263 = 1.0;
				else if( dotResult54_g263 < 0.0 )
				ifLocalVar50_g263 = -1.0;
				float3 appendResult37_g263 = (float3(ifLocalVar50_g263 , -1.0 , 1.0));
				float4 lerpResult27_g263 = lerp( ( tex2D( _Layer02Normal, (temp_output_59_0_g263).xz ) * float4( appendResult38_g263 , 0.0 ) ) , ( tex2D( _Layer02Normal, (temp_output_59_0_g263).yz ) * float4( appendResult37_g263 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g261 ));
				float3 break5_g270 = lerpResult27_g263.rgb;
				float3 temp_output_10_0_g268 = cross( temp_output_5_0_g266 , normalizeResult6_g266 );
				float dotResult5_g268 = dot( temp_output_10_0_g268 , temp_output_10_0_g268 );
				float3 normalizeResult7_g268 = normalize( temp_output_10_0_g268 );
				float2 appendResult8_g268 = (float2(normalizeResult7_g268.x , 0.0));
				float2 appendResult11_g268 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g268 = 0;
				if( dotResult5_g268 <= 1E-06 )
				ifLocalVar4_g268 = appendResult11_g268;
				else
				ifLocalVar4_g268 = appendResult8_g268;
				float3 temp_output_25_0_g263 = ( ( float3( (ifLocalVar4_g267).xy ,  0.0 ) * break5_g270.x ) + ( float3( (ifLocalVar4_g268).xy ,  0.0 ) * break5_g270.y ) + ( normalizeResult6_g266 * break5_g270.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g271 = normalize( temp_output_22_0_g263 );
				float3 normalizeResult7_g271 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g271 = cross( normalizeResult6_g271 , normalizeResult7_g271 );
				float3 temp_output_10_0_g272 = temp_output_5_0_g271;
				float dotResult5_g272 = dot( temp_output_10_0_g272 , temp_output_10_0_g272 );
				float3 normalizeResult7_g272 = normalize( temp_output_10_0_g272 );
				float2 appendResult8_g272 = (float2(normalizeResult7_g272.x , 0.0));
				float2 appendResult11_g272 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g272 = 0;
				if( dotResult5_g272 <= 1E-06 )
				ifLocalVar4_g272 = appendResult11_g272;
				else
				ifLocalVar4_g272 = appendResult8_g272;
				float dotResult20_g263 = dot( temp_output_22_0_g263 , float3(0,0,1) );
				float ifLocalVar16_g263 = 0;
				if( dotResult20_g263 > 0.0 )
				ifLocalVar16_g263 = 1.0;
				else if( dotResult20_g263 < 0.0 )
				ifLocalVar16_g263 = -1.0;
				float3 appendResult14_g263 = (float3(ifLocalVar16_g263 , -1.0 , 1.0));
				float3 break5_g264 = ( tex2D( _Layer02Normal, (temp_output_59_0_g263).xy ) * float4( appendResult14_g263 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g273 = cross( temp_output_5_0_g271 , normalizeResult6_g271 );
				float dotResult5_g273 = dot( temp_output_10_0_g273 , temp_output_10_0_g273 );
				float3 normalizeResult7_g273 = normalize( temp_output_10_0_g273 );
				float2 appendResult8_g273 = (float2(normalizeResult7_g273.x , 0.0));
				float2 appendResult11_g273 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g273 = 0;
				if( dotResult5_g273 <= 1E-06 )
				ifLocalVar4_g273 = appendResult11_g273;
				else
				ifLocalVar4_g273 = appendResult8_g273;
				float3 temp_output_9_0_g263 = ( ( float3( (ifLocalVar4_g272).xy ,  0.0 ) * break5_g264.x ) + ( float3( (ifLocalVar4_g273).xy ,  0.0 ) * break5_g264.y ) + ( normalizeResult6_g271 * break5_g264.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g263 = ( 1.0 - temp_output_59_0_g261 );
				float3 lerpResult5_g263 = lerp( temp_output_25_0_g263 , temp_output_9_0_g263 , temp_output_33_0_g263);
				float3 temp_output_18_0_g261 = ( (float)temp_output_75_0_g261 == 0.0 ? normalizeResult20_g261 : lerpResult5_g263 );
				float3 worldToTangentDir11_g261 = mul( ase_worldToTangent, temp_output_18_0_g261);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch88 = ( (float)temp_output_76_0_g261 == 0.0 ? worldToTangentDir11_g261 : temp_output_18_0_g261 );
				#else
				float3 staticSwitch88 = UnpackNormalScale( tex2D( _Layer02Normal, temp_output_106_0 ), 1.0f );
				#endif
				float3 lerpResult1_g317 = lerp( staticSwitch88 , float3(0,0,1) , _Layer02NormalFlatten);
				float3 lerpResult46 = lerp( lerpResult1_g316 , lerpResult1_g317 , temp_output_50_0);
				int temp_output_76_0_g275 = 0;
				int temp_output_75_0_g275 = (int)1.0;
				float3 temp_output_72_0_g275 = WorldPosition;
				float temp_output_69_0_g275 = ( abs( ( _Layer03Tiling * 5.12 ) ) * -1.0 );
				float3 temp_output_68_0_g275 = ( temp_output_72_0_g275 / temp_output_69_0_g275 );
				float4 tex2DNode27_g275 = tex2D( _Layer03Normal, (temp_output_68_0_g275).xz );
				float3 tangentToWorldDir63_g275 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g275 = ( tangentToWorldDir63_g275 * float3( 1,0,0 ) );
				float3 appendResult35_g275 = (float3(( tex2DNode27_g275.r * -1.0 ) , ( tex2DNode27_g275.b * (temp_output_54_0_g275).y ) , ( tex2DNode27_g275.g * -1.0 )));
				float4 tex2DNode28_g275 = tex2D( _Layer03Normal, (temp_output_68_0_g275).yz );
				float temp_output_51_0_g275 = (temp_output_54_0_g275).x;
				float3 appendResult36_g275 = (float3(( tex2DNode28_g275.r * -1.0 ) , ( tex2DNode28_g275.b * temp_output_51_0_g275 ) , ( tex2DNode28_g275.g * -1.0 )));
				float temp_output_1_0_g276 = 0.0;
				float lerpResult5_g276 = lerp( ( 0.0 - temp_output_1_0_g276 ) , ( temp_output_1_0_g276 + 1.0 ) , abs( temp_output_51_0_g275 ));
				float temp_output_58_0_g275 = saturate( lerpResult5_g276 );
				float3 lerpResult24_g275 = lerp( appendResult35_g275 , appendResult36_g275 , temp_output_58_0_g275);
				float4 tex2DNode29_g275 = tex2D( _Layer03Normal, (temp_output_68_0_g275).xy );
				float temp_output_53_0_g275 = (temp_output_54_0_g275).z;
				float3 appendResult50_g275 = (float3(( (tex2DNode29_g275).rg * float2( -1,-1 ) ) , ( tex2DNode29_g275.b * temp_output_53_0_g275 )));
				float temp_output_1_0_g288 = 0.5;
				float lerpResult5_g288 = lerp( ( 0.0 - temp_output_1_0_g288 ) , ( temp_output_1_0_g288 + 1.0 ) , abs( temp_output_53_0_g275 ));
				float temp_output_59_0_g275 = saturate( lerpResult5_g288 );
				float3 lerpResult25_g275 = lerp( lerpResult24_g275 , appendResult50_g275 , temp_output_59_0_g275);
				float3 normalizeResult20_g275 = normalize( lerpResult25_g275 );
				float3 temp_output_22_0_g277 = WorldNormal;
				float3 normalizeResult6_g280 = normalize( temp_output_22_0_g277 );
				float3 normalizeResult7_g280 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g280 = cross( normalizeResult6_g280 , normalizeResult7_g280 );
				float3 temp_output_10_0_g281 = temp_output_5_0_g280;
				float dotResult5_g281 = dot( temp_output_10_0_g281 , temp_output_10_0_g281 );
				float3 normalizeResult7_g281 = normalize( temp_output_10_0_g281 );
				float2 appendResult8_g281 = (float2(normalizeResult7_g281.x , 0.0));
				float2 appendResult11_g281 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g281 = 0;
				if( dotResult5_g281 <= 1E-06 )
				ifLocalVar4_g281 = appendResult11_g281;
				else
				ifLocalVar4_g281 = appendResult8_g281;
				float3 temp_cast_68 = (temp_output_69_0_g275).xxx;
				float3 temp_output_59_0_g277 = ( temp_output_72_0_g275 / temp_cast_68 );
				float dotResult49_g277 = dot( temp_output_22_0_g277 , float3(0,1,0) );
				float ifLocalVar45_g277 = 0;
				if( dotResult49_g277 > 0.0 )
				ifLocalVar45_g277 = -1.0;
				else if( dotResult49_g277 < 0.0 )
				ifLocalVar45_g277 = 1.0;
				float3 appendResult38_g277 = (float3(ifLocalVar45_g277 , -1.0 , 1.0));
				float dotResult54_g277 = dot( temp_output_22_0_g277 , float3(1,0,0) );
				float ifLocalVar50_g277 = 0;
				if( dotResult54_g277 > 0.0 )
				ifLocalVar50_g277 = 1.0;
				else if( dotResult54_g277 < 0.0 )
				ifLocalVar50_g277 = -1.0;
				float3 appendResult37_g277 = (float3(ifLocalVar50_g277 , -1.0 , 1.0));
				float4 lerpResult27_g277 = lerp( ( tex2D( _Layer03Normal, (temp_output_59_0_g277).xz ) * float4( appendResult38_g277 , 0.0 ) ) , ( tex2D( _Layer03Normal, (temp_output_59_0_g277).yz ) * float4( appendResult37_g277 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g275 ));
				float3 break5_g284 = lerpResult27_g277.rgb;
				float3 temp_output_10_0_g282 = cross( temp_output_5_0_g280 , normalizeResult6_g280 );
				float dotResult5_g282 = dot( temp_output_10_0_g282 , temp_output_10_0_g282 );
				float3 normalizeResult7_g282 = normalize( temp_output_10_0_g282 );
				float2 appendResult8_g282 = (float2(normalizeResult7_g282.x , 0.0));
				float2 appendResult11_g282 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g282 = 0;
				if( dotResult5_g282 <= 1E-06 )
				ifLocalVar4_g282 = appendResult11_g282;
				else
				ifLocalVar4_g282 = appendResult8_g282;
				float3 temp_output_25_0_g277 = ( ( float3( (ifLocalVar4_g281).xy ,  0.0 ) * break5_g284.x ) + ( float3( (ifLocalVar4_g282).xy ,  0.0 ) * break5_g284.y ) + ( normalizeResult6_g280 * break5_g284.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g285 = normalize( temp_output_22_0_g277 );
				float3 normalizeResult7_g285 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g285 = cross( normalizeResult6_g285 , normalizeResult7_g285 );
				float3 temp_output_10_0_g286 = temp_output_5_0_g285;
				float dotResult5_g286 = dot( temp_output_10_0_g286 , temp_output_10_0_g286 );
				float3 normalizeResult7_g286 = normalize( temp_output_10_0_g286 );
				float2 appendResult8_g286 = (float2(normalizeResult7_g286.x , 0.0));
				float2 appendResult11_g286 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g286 = 0;
				if( dotResult5_g286 <= 1E-06 )
				ifLocalVar4_g286 = appendResult11_g286;
				else
				ifLocalVar4_g286 = appendResult8_g286;
				float dotResult20_g277 = dot( temp_output_22_0_g277 , float3(0,0,1) );
				float ifLocalVar16_g277 = 0;
				if( dotResult20_g277 > 0.0 )
				ifLocalVar16_g277 = 1.0;
				else if( dotResult20_g277 < 0.0 )
				ifLocalVar16_g277 = -1.0;
				float3 appendResult14_g277 = (float3(ifLocalVar16_g277 , -1.0 , 1.0));
				float3 break5_g278 = ( tex2D( _Layer03Normal, (temp_output_59_0_g277).xy ) * float4( appendResult14_g277 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g287 = cross( temp_output_5_0_g285 , normalizeResult6_g285 );
				float dotResult5_g287 = dot( temp_output_10_0_g287 , temp_output_10_0_g287 );
				float3 normalizeResult7_g287 = normalize( temp_output_10_0_g287 );
				float2 appendResult8_g287 = (float2(normalizeResult7_g287.x , 0.0));
				float2 appendResult11_g287 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g287 = 0;
				if( dotResult5_g287 <= 1E-06 )
				ifLocalVar4_g287 = appendResult11_g287;
				else
				ifLocalVar4_g287 = appendResult8_g287;
				float3 temp_output_9_0_g277 = ( ( float3( (ifLocalVar4_g286).xy ,  0.0 ) * break5_g278.x ) + ( float3( (ifLocalVar4_g287).xy ,  0.0 ) * break5_g278.y ) + ( normalizeResult6_g285 * break5_g278.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g277 = ( 1.0 - temp_output_59_0_g275 );
				float3 lerpResult5_g277 = lerp( temp_output_25_0_g277 , temp_output_9_0_g277 , temp_output_33_0_g277);
				float3 temp_output_18_0_g275 = ( (float)temp_output_75_0_g275 == 0.0 ? normalizeResult20_g275 : lerpResult5_g277 );
				float3 worldToTangentDir11_g275 = mul( ase_worldToTangent, temp_output_18_0_g275);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch89 = ( (float)temp_output_76_0_g275 == 0.0 ? worldToTangentDir11_g275 : temp_output_18_0_g275 );
				#else
				float3 staticSwitch89 = UnpackNormalScale( tex2D( _Layer03Normal, temp_output_107_0 ), 1.0f );
				#endif
				float3 lerpResult1_g318 = lerp( staticSwitch89 , float3(0,0,1) , _Layer03NormalFlatten);
				float3 lerpResult47 = lerp( lerpResult46 , lerpResult1_g318 , temp_output_51_0);
				int temp_output_76_0_g294 = 0;
				int temp_output_75_0_g294 = (int)1.0;
				float3 temp_output_72_0_g294 = WorldPosition;
				float temp_output_69_0_g294 = ( abs( ( _Layer04Tiling * 512.0 ) ) * -1.0 );
				float3 temp_output_68_0_g294 = ( temp_output_72_0_g294 / temp_output_69_0_g294 );
				float4 tex2DNode27_g294 = tex2D( _Layer04Normal, (temp_output_68_0_g294).xz );
				float3 tangentToWorldDir63_g294 = mul( ase_tangentToWorldFast, float3( 0,0,1 ) );
				float3 temp_output_54_0_g294 = ( tangentToWorldDir63_g294 * float3( 1,0,0 ) );
				float3 appendResult35_g294 = (float3(( tex2DNode27_g294.r * -1.0 ) , ( tex2DNode27_g294.b * (temp_output_54_0_g294).y ) , ( tex2DNode27_g294.g * -1.0 )));
				float4 tex2DNode28_g294 = tex2D( _Layer04Normal, (temp_output_68_0_g294).yz );
				float temp_output_51_0_g294 = (temp_output_54_0_g294).x;
				float3 appendResult36_g294 = (float3(( tex2DNode28_g294.r * -1.0 ) , ( tex2DNode28_g294.b * temp_output_51_0_g294 ) , ( tex2DNode28_g294.g * -1.0 )));
				float temp_output_1_0_g295 = 0.0;
				float lerpResult5_g295 = lerp( ( 0.0 - temp_output_1_0_g295 ) , ( temp_output_1_0_g295 + 1.0 ) , abs( temp_output_51_0_g294 ));
				float temp_output_58_0_g294 = saturate( lerpResult5_g295 );
				float3 lerpResult24_g294 = lerp( appendResult35_g294 , appendResult36_g294 , temp_output_58_0_g294);
				float4 tex2DNode29_g294 = tex2D( _Layer04Normal, (temp_output_68_0_g294).xy );
				float temp_output_53_0_g294 = (temp_output_54_0_g294).z;
				float3 appendResult50_g294 = (float3(( (tex2DNode29_g294).rg * float2( -1,-1 ) ) , ( tex2DNode29_g294.b * temp_output_53_0_g294 )));
				float temp_output_1_0_g307 = 0.5;
				float lerpResult5_g307 = lerp( ( 0.0 - temp_output_1_0_g307 ) , ( temp_output_1_0_g307 + 1.0 ) , abs( temp_output_53_0_g294 ));
				float temp_output_59_0_g294 = saturate( lerpResult5_g307 );
				float3 lerpResult25_g294 = lerp( lerpResult24_g294 , appendResult50_g294 , temp_output_59_0_g294);
				float3 normalizeResult20_g294 = normalize( lerpResult25_g294 );
				float3 temp_output_22_0_g296 = WorldNormal;
				float3 normalizeResult6_g299 = normalize( temp_output_22_0_g296 );
				float3 normalizeResult7_g299 = normalize( float3( 0,0,1 ) );
				float3 temp_output_5_0_g299 = cross( normalizeResult6_g299 , normalizeResult7_g299 );
				float3 temp_output_10_0_g300 = temp_output_5_0_g299;
				float dotResult5_g300 = dot( temp_output_10_0_g300 , temp_output_10_0_g300 );
				float3 normalizeResult7_g300 = normalize( temp_output_10_0_g300 );
				float2 appendResult8_g300 = (float2(normalizeResult7_g300.x , 0.0));
				float2 appendResult11_g300 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g300 = 0;
				if( dotResult5_g300 <= 1E-06 )
				ifLocalVar4_g300 = appendResult11_g300;
				else
				ifLocalVar4_g300 = appendResult8_g300;
				float3 temp_cast_90 = (temp_output_69_0_g294).xxx;
				float3 temp_output_59_0_g296 = ( temp_output_72_0_g294 / temp_cast_90 );
				float dotResult49_g296 = dot( temp_output_22_0_g296 , float3(0,1,0) );
				float ifLocalVar45_g296 = 0;
				if( dotResult49_g296 > 0.0 )
				ifLocalVar45_g296 = -1.0;
				else if( dotResult49_g296 < 0.0 )
				ifLocalVar45_g296 = 1.0;
				float3 appendResult38_g296 = (float3(ifLocalVar45_g296 , -1.0 , 1.0));
				float dotResult54_g296 = dot( temp_output_22_0_g296 , float3(1,0,0) );
				float ifLocalVar50_g296 = 0;
				if( dotResult54_g296 > 0.0 )
				ifLocalVar50_g296 = 1.0;
				else if( dotResult54_g296 < 0.0 )
				ifLocalVar50_g296 = -1.0;
				float3 appendResult37_g296 = (float3(ifLocalVar50_g296 , -1.0 , 1.0));
				float4 lerpResult27_g296 = lerp( ( tex2D( _Layer04Normal, (temp_output_59_0_g296).xz ) * float4( appendResult38_g296 , 0.0 ) ) , ( tex2D( _Layer04Normal, (temp_output_59_0_g296).yz ) * float4( appendResult37_g296 , 0.0 ) ) , ( 1.0 - temp_output_58_0_g294 ));
				float3 break5_g303 = lerpResult27_g296.rgb;
				float3 temp_output_10_0_g301 = cross( temp_output_5_0_g299 , normalizeResult6_g299 );
				float dotResult5_g301 = dot( temp_output_10_0_g301 , temp_output_10_0_g301 );
				float3 normalizeResult7_g301 = normalize( temp_output_10_0_g301 );
				float2 appendResult8_g301 = (float2(normalizeResult7_g301.x , 0.0));
				float2 appendResult11_g301 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g301 = 0;
				if( dotResult5_g301 <= 1E-06 )
				ifLocalVar4_g301 = appendResult11_g301;
				else
				ifLocalVar4_g301 = appendResult8_g301;
				float3 temp_output_25_0_g296 = ( ( float3( (ifLocalVar4_g300).xy ,  0.0 ) * break5_g303.x ) + ( float3( (ifLocalVar4_g301).xy ,  0.0 ) * break5_g303.y ) + ( normalizeResult6_g299 * break5_g303.z ) + float3( 0,0,0 ) );
				float3 normalizeResult6_g304 = normalize( temp_output_22_0_g296 );
				float3 normalizeResult7_g304 = normalize( float3(0,1,0) );
				float3 temp_output_5_0_g304 = cross( normalizeResult6_g304 , normalizeResult7_g304 );
				float3 temp_output_10_0_g305 = temp_output_5_0_g304;
				float dotResult5_g305 = dot( temp_output_10_0_g305 , temp_output_10_0_g305 );
				float3 normalizeResult7_g305 = normalize( temp_output_10_0_g305 );
				float2 appendResult8_g305 = (float2(normalizeResult7_g305.x , 0.0));
				float2 appendResult11_g305 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g305 = 0;
				if( dotResult5_g305 <= 1E-06 )
				ifLocalVar4_g305 = appendResult11_g305;
				else
				ifLocalVar4_g305 = appendResult8_g305;
				float dotResult20_g296 = dot( temp_output_22_0_g296 , float3(0,0,1) );
				float ifLocalVar16_g296 = 0;
				if( dotResult20_g296 > 0.0 )
				ifLocalVar16_g296 = 1.0;
				else if( dotResult20_g296 < 0.0 )
				ifLocalVar16_g296 = -1.0;
				float3 appendResult14_g296 = (float3(ifLocalVar16_g296 , -1.0 , 1.0));
				float3 break5_g297 = ( tex2D( _Layer04Normal, (temp_output_59_0_g296).xy ) * float4( appendResult14_g296 , 0.0 ) ).rgb;
				float3 temp_output_10_0_g306 = cross( temp_output_5_0_g304 , normalizeResult6_g304 );
				float dotResult5_g306 = dot( temp_output_10_0_g306 , temp_output_10_0_g306 );
				float3 normalizeResult7_g306 = normalize( temp_output_10_0_g306 );
				float2 appendResult8_g306 = (float2(normalizeResult7_g306.x , 0.0));
				float2 appendResult11_g306 = (float2(float3( 0,0,0 ).x , 1.0));
				float2 ifLocalVar4_g306 = 0;
				if( dotResult5_g306 <= 1E-06 )
				ifLocalVar4_g306 = appendResult11_g306;
				else
				ifLocalVar4_g306 = appendResult8_g306;
				float3 temp_output_9_0_g296 = ( ( float3( (ifLocalVar4_g305).xy ,  0.0 ) * break5_g297.x ) + ( float3( (ifLocalVar4_g306).xy ,  0.0 ) * break5_g297.y ) + ( normalizeResult6_g304 * break5_g297.z ) + float3( 0,0,0 ) );
				float temp_output_33_0_g296 = ( 1.0 - temp_output_59_0_g294 );
				float3 lerpResult5_g296 = lerp( temp_output_25_0_g296 , temp_output_9_0_g296 , temp_output_33_0_g296);
				float3 temp_output_18_0_g294 = ( (float)temp_output_75_0_g294 == 0.0 ? normalizeResult20_g294 : lerpResult5_g296 );
				float3 worldToTangentDir11_g294 = mul( ase_worldToTangent, temp_output_18_0_g294);
				#ifdef _SWITCHTOWORLDTRIPLANAR_ON
				float3 staticSwitch90 = ( (float)temp_output_76_0_g294 == 0.0 ? worldToTangentDir11_g294 : temp_output_18_0_g294 );
				#else
				float3 staticSwitch90 = UnpackNormalScale( tex2D( _Layer04Normal, temp_output_108_0 ), 1.0f );
				#endif
				float3 lerpResult1_g319 = lerp( staticSwitch90 , float3(0,0,1) , _Layer04NormalFlatten);
				float3 lerpResult48 = lerp( lerpResult47 , lerpResult1_g319 , temp_output_52_0);
				float3 lerpResult19 = lerp( lerpResult48 , staticSwitch91 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float3 staticSwitch12 = lerpResult19;
				#else
				float3 staticSwitch12 = lerpResult48;
				#endif
				float2 texCoord10 = IN.ase_texcoord7.xy * float2( 5,5 ) + float2( 0,0 );
				
				float lerpResult38 = lerp( _Layer1Spec , _Layer2Spec , temp_output_50_0);
				float lerpResult39 = lerp( lerpResult38 , _Layer3Spec , temp_output_51_0);
				float lerpResult40 = lerp( lerpResult39 , _Layer4Spec , temp_output_52_0);
				float3 temp_cast_106 = (lerpResult40).xxx;
				
				float4 lerpResult28 = lerp( staticSwitch82 , staticSwitch83 , temp_output_50_0);
				float4 lerpResult29 = lerp( lerpResult28 , staticSwitch84 , temp_output_51_0);
				float4 lerpResult30 = lerp( lerpResult29 , staticSwitch85 , temp_output_52_0);
				float lerpResult33 = lerp( _RoughnessMin , _RoughnessMax , (lerpResult30).r);
				float lerpResult18 = lerp( lerpResult33 , temp_output_74_0 , temp_output_26_0);
				#ifdef _BLENDON_ON
				float staticSwitch13 = lerpResult18;
				#else
				float staticSwitch13 = lerpResult33;
				#endif
				
				float3 Albedo = staticSwitch15.rgb;
				float3 Normal = ( staticSwitch12 + UnpackNormalScale( tex2D( _TerrainNormal, texCoord10 ), 1.0f ) );
				float3 Emission = 0;
				float3 Specular = temp_cast_106;
				float Metallic = 0;
				float Smoothness = ( 1.0 - staticSwitch13 );
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif

				BRDFData brdfData;
				InitializeBRDFData( Albedo, Metallic, Specular, Smoothness, Alpha, brdfData);
				half4 color;
				color.rgb = GlobalIllumination( brdfData, inputData.bakedGI, Occlusion, inputData.normalWS, inputData.viewDirectionWS);
				color.a = Alpha;

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;
				
					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;
				
					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );
				
							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif
				
				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;
				
					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
				
					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;
				
					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );
				
							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif
				
				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, float4( WorldNormal, 0 ) ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos.xy ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif
				
				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif
				
				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				
				return BRDFDataToGbuffer(brdfData, inputData, Smoothness, Emission + color.rgb);
			}

			ENDHLSL
		}
		
	}
	
	CustomEditor "UnityEditor.ShaderGraphLitGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18935
300;73;1124;649;8213.289;-869.8036;2.409001;True;False
Node;AmplifyShaderEditor.CommentaryNode;153;-14214.87,449.9083;Inherit;False;3277.457;844.199;Flowmap UV;28;151;150;145;146;144;149;143;148;147;142;140;139;133;132;138;137;131;136;135;125;134;127;126;128;129;124;152;203;Flowmap UV;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;152;-14163.87,1155.107;Inherit;False;Property;_FlowUVScale;Flow UV Scale;11;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;151;-13987.87,1155.107;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;512;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;150;-13843.87,1155.107;Inherit;False;SH_UEWorldPosition-XY;-1;;81;111f0f055447f1742a757c7ecbbd9da7;0;1;4;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;146;-13812.87,600.1072;Inherit;False;Property;_DISTANCE_FIELD_NOT_SUPPORTED;DISTANCE_FIELD_NOT_SUPPORTED;0;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;145;-13774.48,697.9851;Inherit;False;Property;_DistanceFieldUVScale;Distance Field UV Scale;9;0;Create;True;0;0;0;False;0;False;0.32;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;203;-13915.96,911.7086;Inherit;True;Property;_Texture3;Texture 3;56;1;[HideInInspector];Create;True;0;0;0;False;0;False;e5916bc899761fe40a5062fb04275aee;e5916bc899761fe40a5062fb04275aee;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SamplerNode;149;-13588.87,1064.107;Inherit;True;Property;_FlowMap;Flow Map;29;0;Create;True;0;0;0;False;0;False;-1;e5916bc899761fe40a5062fb04275aee;e5916bc899761fe40a5062fb04275aee;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleDivideOpNode;144;-13476.87,632.1072;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;148;-13204.87,1064.107;Inherit;False;True;True;False;False;1;0;COLOR;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TransformDirectionNode;143;-13332.87,632.1072;Inherit;False;World;Object;False;Fast;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleDivideOpNode;147;-12981.54,1067.798;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;32,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ComponentMaskNode;142;-13076.87,632.1072;Inherit;False;True;True;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;140;-12724.87,696.1072;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;133;-12580.87,1144.107;Inherit;False;Property;_UVWorldXYScale;UV World XY Scale;10;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;139;-12564.87,696.1072;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;132;-12340.87,1144.107;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;512;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;138;-12372.87,696.1072;Inherit;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;137;-12404.87,792.1071;Inherit;False;Property;_FlowStrength;Flow Strength;13;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;135;-12166.48,879.9849;Inherit;False;Property;_FlowBlend;Flow Blend;12;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;131;-12180.87,1144.107;Inherit;False;SH_UEWorldPosition-XY;-1;;82;111f0f055447f1742a757c7ecbbd9da7;0;1;4;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;136;-12148.87,744.1072;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;202;-6852.887,-649.8611;Inherit;False;1545.417;515.1199;Comment;18;196;200;198;205;197;204;186;189;185;192;191;188;184;195;187;190;194;193;Vertex color to lerp the height components of the textures;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;123;-10472.87,582.1072;Inherit;False;552;598;Lerp the flowmap intensity;8;118;120;121;114;115;122;116;117;Flowmap Intensity;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;127;-11588.06,933.7198;Inherit;False;Constant;_FlowmapTextureBias;Flowmap Texture Bias;44;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;125;-11613.04,499.9082;Inherit;True;Property;_Texture2;Texture 2;14;0;Create;True;0;0;0;False;0;False;e5916bc899761fe40a5062fb04275aee;e5916bc899761fe40a5062fb04275aee;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;129;-11590.04,773.9082;Inherit;False;Constant;_FlowmapUseMipLevel;Flowmap UseMipLevel;44;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;128;-11590.06,853.7207;Inherit;False;Constant;_FlowmapTextureSize;Flowmap Texture Size;44;0;Create;True;0;0;0;False;0;False;5.12;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;134;-11907.48,842.985;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;126;-11542.06,693.7212;Inherit;False;Constant;_FlowmapTime;Flowmap Time;44;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;197;-6793.094,-539.2078;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;113;-9624.866,582.1072;Inherit;False;516;614;Tiling the flowmap UV;8;106;105;107;108;109;110;111;112;Flowmap Tiling;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;204;-6799.003,-362.2524;Inherit;False;Property;_BlendsOffset;Blends Offset;26;0;Create;True;0;0;0;False;0;False;0.1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;118;-10422.87,680.1072;Inherit;False;Property;_Layer01FlowIntensity;Layer 01 Flow Intensity;8;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;124;-11300.41,573.1728;Inherit;False;SH_UEFlowmaps;4;;83;2bacb256711f8e747822b1b1e7e19f16;0;9;46;SAMPLER2D;;False;47;SAMPLER2D;;False;14;FLOAT2;0.1,0;False;52;FLOAT2;0.5,0.5;False;11;FLOAT;0;False;90;INT;1;False;21;FLOAT2;0,0;False;25;FLOAT2;1024,1024;False;27;FLOAT;0;False;5;FLOAT3;72;FLOAT;73;COLOR;67;FLOAT4;63;FLOAT2;62
Node;AmplifyShaderEditor.RangedFloatNode;120;-10422.87,808.1071;Inherit;False;Constant;_Layer02FlowIntensity;Layer 02 Flow Intensity;20;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;182;-9035.16,-2546;Inherit;False;1760.16;920.4768;Comment;17;176;175;174;169;172;168;180;171;173;179;170;167;164;166;177;178;225;World Triplanar UV and Tiling - Base Color;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;115;-10102.87,760.1072;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;198;-6560.094,-460.2074;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;121;-10422.87,936.1068;Inherit;False;Constant;_Layer03FlowIntensity;Layer 03 Flow Intensity;21;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;114;-10102.87,632.1072;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;205;-6581.688,-356.0485;Inherit;False;Property;_BlendsDivisor;Blends Divisor;25;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;225;-8282.068,-2351.189;Inherit;False;Constant;_Float10;Float 10;65;0;Create;True;0;0;0;False;0;False;5.12;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;110;-9574.866,824.1071;Inherit;False;Property;_Layer02Tiling;Layer 02 Tiling;53;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;109;-9574.866,696.1072;Inherit;False;Property;_Layer01Tiling;Layer 01 Tiling;52;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;169;-8983.145,-2468.958;Inherit;True;Property;_Layer01RAOHMTexture;Layer 01 RAOHM Texture;30;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;d0ec1dbd8d7137a48b0ef6b83149246d;d0ec1dbd8d7137a48b0ef6b83149246d;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.TexturePropertyNode;174;-8983.145,-2260.957;Inherit;True;Property;_Layer02RAOHMTexture;Layer 02 RAOHM Texture;32;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;9b3604fad6dfa3f4a9da853a77c1655f;9b3604fad6dfa3f4a9da853a77c1655f;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SimpleDivideOpNode;200;-6400.094,-460.2074;Inherit;False;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;166;-7872,-2144;Inherit;False;Constant;_Float3;Float 3;59;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;162;-7461.556,-1476.959;Inherit;False;370;904;Comment;4;159;158;160;161;Non Triplanar UV;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;106;-9270.866,760.1072;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;177;-8016,-2448;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;512;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;178;-8016,-2240;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;512;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;122;-10422.87,1064.107;Inherit;False;Constant;_Layer04FlowIntensity;Layer 04 Flow Intensity;22;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;116;-10102.87,888.1069;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;111;-9574.866,952.1068;Inherit;False;Property;_Layer03Tiling;Layer 03 Tiling;54;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;105;-9270.866,632.1072;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;164;-7872,-2352;Inherit;False;Constant;_Float7;Float 7;59;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;158;-7411.556,-1426.959;Inherit;True;Property;_Layer01RAOHM;Layer 01 RAOHM;48;0;Create;True;0;0;0;False;0;False;-1;d0ec1dbd8d7137a48b0ef6b83149246d;d0ec1dbd8d7137a48b0ef6b83149246d;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;112;-9574.866,1080.107;Inherit;False;Property;_Layer04Tiling;Layer 04 Tiling;55;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;173;-7712,-2496;Inherit;False;SH_UEWorldAlignedTexture;1;;94;a8a2d3d2fa5a3f14f8223bf42295e825;0;6;36;SAMPLER2D;;False;45;FLOAT3;0.64,0.64,0.64;False;42;FLOAT3;0,0,0;False;23;FLOAT;0;False;15;FLOAT3;0,0,0;False;9;FLOAT;1;False;3;COLOR;1;COLOR;0;COLOR;2
Node;AmplifyShaderEditor.FunctionNode;170;-7712,-2288;Inherit;False;SH_UEWorldAlignedTexture;1;;91;a8a2d3d2fa5a3f14f8223bf42295e825;0;6;36;SAMPLER2D;;False;45;FLOAT3;0.64,0.64,0.64;False;42;FLOAT3;0,0,0;False;23;FLOAT;0;False;15;FLOAT3;0,0,0;False;9;FLOAT;1;False;3;COLOR;1;COLOR;0;COLOR;2
Node;AmplifyShaderEditor.BreakToComponentsNode;196;-6256.094,-460.2074;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.TexturePropertyNode;175;-8983.145,-2052.957;Inherit;True;Property;_Layer03RAOHMTexture;Layer 03 RAOHM Texture;34;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;9b3604fad6dfa3f4a9da853a77c1655f;9b3604fad6dfa3f4a9da853a77c1655f;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SamplerNode;159;-7411.556,-1218.959;Inherit;True;Property;_Layer02RAOHM;Layer 02 RAOHM;49;0;Create;True;0;0;0;False;0;False;-1;9b3604fad6dfa3f4a9da853a77c1655f;9b3604fad6dfa3f4a9da853a77c1655f;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;117;-10102.87,1016.107;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;183;-6514,-2066;Inherit;False;837;622;Comment;12;83;82;84;75;76;74;85;77;78;79;80;81;Switch to world triplanar UV mapping;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;167;-7872,-1936;Inherit;False;Constant;_Float5;Float 5;59;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;179;-8016,-2032;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;512;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;107;-9270.866,888.1069;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;171;-7712,-2080;Inherit;False;SH_UEWorldAlignedTexture;1;;97;a8a2d3d2fa5a3f14f8223bf42295e825;0;6;36;SAMPLER2D;;False;45;FLOAT3;0.64,0.64,0.64;False;42;FLOAT3;0,0,0;False;23;FLOAT;0;False;15;FLOAT3;0,0,0;False;9;FLOAT;1;False;3;COLOR;1;COLOR;0;COLOR;2
Node;AmplifyShaderEditor.RangedFloatNode;168;-7872,-1728;Inherit;False;Constant;_Float8;Float 8;59;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;108;-9270.866,1016.107;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;180;-8016,-1824;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;512;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;82;-6464,-2016;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;15;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;193;-6039.034,-559.0859;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;160;-7411.556,-1010.96;Inherit;True;Property;_Layer03RAOHM;Layer 03 RAOHM;47;0;Create;True;0;0;0;False;0;False;-1;9b3604fad6dfa3f4a9da853a77c1655f;9b3604fad6dfa3f4a9da853a77c1655f;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;176;-8983.145,-1844.956;Inherit;True;Property;_Layer04RAOHMTexture;Layer 04 RAOHM Texture;36;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;d0ec1dbd8d7137a48b0ef6b83149246d;d0ec1dbd8d7137a48b0ef6b83149246d;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.StaticSwitch;83;-6464,-1872;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;28;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;194;-6039.034,-431.0856;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;76;-6096,-1872;Inherit;False;False;False;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;73;-5316.371,-1859.277;Inherit;False;1489.436;716.661;Belnd the height based on the adjusted vertex color;6;52;55;51;54;50;53;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;187;-5655.034,-495.0856;Inherit;False;Property;_Blend01Bias;Blend 01 Bias;15;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;84;-6464,-1728;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;20;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;172;-7712,-1872;Inherit;False;SH_UEWorldAlignedTexture;1;;100;a8a2d3d2fa5a3f14f8223bf42295e825;0;6;36;SAMPLER2D;;False;45;FLOAT3;0.64,0.64,0.64;False;42;FLOAT3;0,0,0;False;23;FLOAT;0;False;15;FLOAT3;0,0,0;False;9;FLOAT;1;False;3;COLOR;1;COLOR;0;COLOR;2
Node;AmplifyShaderEditor.OneMinusNode;190;-5863.034,-559.0859;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;161;-7411.556,-802.9602;Inherit;True;Property;_Layer04RAOHM;Layer 04 RAOHM;50;0;Create;True;0;0;0;False;0;False;-1;d0ec1dbd8d7137a48b0ef6b83149246d;d0ec1dbd8d7137a48b0ef6b83149246d;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;75;-6096,-2016;Inherit;False;False;False;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;214;-10128,-1088;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;5.12;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;184;-5479.034,-559.0859;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;195;-6039.034,-303.0857;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;188;-5655.034,-367.0856;Inherit;False;Property;_Blend02Bias;Blend 02 Bias;17;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;223;-10040.93,-1295.61;Inherit;False;Constant;_Float9;Float 9;65;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;78;-6096,-1728;Inherit;False;False;False;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;85;-6464,-1584;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;22;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.OneMinusNode;74;-5856,-2016;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;53;-5291.04,-1654.313;Inherit;False;Property;_Blend01Contrast;Blend 01 Contrast;16;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;219;-10368,-1152;Inherit;True;Property;_Layer01Normal;Layer 01 Normal;29;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;abf790eb2fc5a2a4da76c3d8356191fd;abf790eb2fc5a2a4da76c3d8356191fd;True;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.TexturePropertyNode;220;-10368,-895;Inherit;True;Property;_Layer02Normal;Layer 02 Normal;31;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;1ae807b896fabcb4eae58a72a9a0eb83;1ae807b896fabcb4eae58a72a9a0eb83;True;bump;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.OneMinusNode;191;-5863.034,-431.0856;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;77;-5856,-1872;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;215;-10128,-832;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;5.12;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;213;-9728,-1152;Inherit;False;SH_UEWorldAlignedNormal;-1;;247;6c195c1fde7cff141ac1995da6115c27;0;7;30;SAMPLER2D;0;False;71;FLOAT;0.64;False;76;INT;0;False;64;FLOAT3;0,0,1;False;60;FLOAT;0.5;False;72;FLOAT3;0,0,0;False;75;INT;1;False;4;FLOAT3;2;FLOAT3;0;FLOAT3;1;FLOAT4;3
Node;AmplifyShaderEditor.OneMinusNode;79;-5856,-1728;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;32;-3530.547,-175.6731;Inherit;False;224.8513;679.7338;Ambien Occlusion Blend;4;86;30;29;28;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;100;-5924.717,556.8059;Inherit;False;569;950;Switch to world triplanar UV mapping - normal;9;88;92;91;90;95;94;93;89;231;;1,1,1,1;0;0
Node;AmplifyShaderEditor.ComponentMaskNode;80;-6096,-1584;Inherit;False;False;False;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;210;-9728,-896;Inherit;False;SH_UEWorldAlignedNormal;-1;;261;6c195c1fde7cff141ac1995da6115c27;0;7;30;SAMPLER2D;0;False;71;FLOAT;0.64;False;76;INT;0;False;64;FLOAT3;0,0,1;False;60;FLOAT;0.5;False;72;FLOAT3;0,0,0;False;75;INT;1;False;4;FLOAT3;2;FLOAT3;0;FLOAT3;1;FLOAT4;3
Node;AmplifyShaderEditor.PowerNode;185;-5479.034,-431.0856;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;189;-5655.034,-239.0857;Inherit;False;Property;_Blend03Bias;Blend 03 Bias;19;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;50;-5095.079,-1809.277;Inherit;False;SF_UEHeightLerpWithTwoHeightMaps;-1;;245;392afb22209c8e44d869498c571e26e4;0;6;8;FLOAT3;1,1,1;False;9;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;4;FLOAT3;7;FLOAT;0;FLOAT;5;FLOAT;6
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;216;-10128,-576;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;5.12;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;221;-10368,-640;Inherit;True;Property;_Layer03Normal;Layer 03 Normal;33;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;32596ec3a324e8c45b443087364b2237;32596ec3a324e8c45b443087364b2237;True;bump;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SamplerNode;102;-6910.337,1275.914;Inherit;True;Property;_TextureSample1;Texture Sample 1;13;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;101;-6908.228,1067.914;Inherit;True;Property;_TextureSample0;Texture Sample 0;12;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.OneMinusNode;192;-5863.034,-303.0857;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;54;-4856.594,-1440.924;Inherit;False;Property;_Blend02Contrast;Blend 02 Contrast;18;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;81;-5856,-1584;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;103;-6910.337,1483.914;Inherit;True;Property;_TextureSample2;Texture Sample 2;14;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;92;-5843.717,718.8057;Inherit;False;Property;_Layer01NormalFlatten;Layer 01 Normal Flatten;21;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;186;-5479.034,-303.0857;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;72;-5471.05,-3841.745;Inherit;False;2551.809;1625.72;Base color LERP between 4 layers of textures;15;66;64;65;67;69;68;60;59;71;70;61;56;62;57;58;Base color lerp between 4 layers of textures;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;93;-5842.717,942.8065;Inherit;False;Property;_Layer02NormalFlatten;Layer 02 Normal Flatten;22;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;28;-3488,-112;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;88;-5874.717,830.8063;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;19;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;217;-10128,-320;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;512;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;211;-9728,-640;Inherit;False;SH_UEWorldAlignedNormal;-1;;275;6c195c1fde7cff141ac1995da6115c27;0;7;30;SAMPLER2D;0;False;71;FLOAT;0.64;False;76;INT;0;False;64;FLOAT3;0,0,1;False;60;FLOAT;0.5;False;72;FLOAT3;0,0,0;False;75;INT;1;False;4;FLOAT3;2;FLOAT3;0;FLOAT3;1;FLOAT4;3
Node;AmplifyShaderEditor.FunctionNode;51;-4660.127,-1594.904;Inherit;False;SF_UEHeightLerpWithTwoHeightMaps;-1;;289;392afb22209c8e44d869498c571e26e4;0;6;8;FLOAT3;1,1,1;False;9;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;4;FLOAT3;7;FLOAT;0;FLOAT;5;FLOAT;6
Node;AmplifyShaderEditor.StaticSwitch;91;-5874.717,606.806;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;17;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;55;-4444.975,-1235.583;Inherit;False;Property;_Blend03Contrast;Blend 03 Contrast;20;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;222;-10368,-384;Inherit;True;Property;_Layer04Normal;Layer 04 Normal;35;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;10c6718601b991d40bc38bfb378a4c7e;10c6718601b991d40bc38bfb378a4c7e;True;bump;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.ColorNode;66;-5421.05,-3407.745;Inherit;False;Property;_Layer02AColor;Layer 02 A Color;40;0;Create;True;0;0;0;False;0;False;1,0,0,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;212;-9728,-384;Inherit;False;SH_UEWorldAlignedNormal;-1;;294;6c195c1fde7cff141ac1995da6115c27;0;7;30;SAMPLER2D;0;False;71;FLOAT;0.64;False;76;INT;0;False;64;FLOAT3;0,0,1;False;60;FLOAT;0.5;False;72;FLOAT3;0,0,0;False;75;INT;1;False;4;FLOAT3;2;FLOAT3;0;FLOAT3;1;FLOAT4;3
Node;AmplifyShaderEditor.StaticSwitch;89;-5874.717,1054.807;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;21;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;29;-3488,64;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;52;-4230.873,-1395.684;Inherit;False;SF_UEHeightLerpWithTwoHeightMaps;-1;;291;392afb22209c8e44d869498c571e26e4;0;6;8;FLOAT3;1,1,1;False;9;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;4;FLOAT3;7;FLOAT;0;FLOAT;5;FLOAT;6
Node;AmplifyShaderEditor.CommentaryNode;37;-3042.19,-441.0772;Inherit;False;909.1814;644.2542;Roughness Blend;8;33;34;36;35;27;26;25;24;;1,1,1,1;0;0
Node;AmplifyShaderEditor.FunctionNode;231;-5607.717,677.8056;Inherit;False;SF_UEFlattenNormal;-1;;316;a744336ecfc2d7743b2d22dfa4eb0cf3;0;2;2;FLOAT3;0,0,1;False;4;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;104;-6910.337,1691.914;Inherit;True;Property;_TextureSample3;Texture Sample 3;15;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;64;-5421.05,-3791.745;Inherit;False;Property;_Layer01AColor;Layer 01 A Color;38;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;232;-5602.717,910.8065;Inherit;False;SF_UEFlattenNormal;-1;;317;a744336ecfc2d7743b2d22dfa4eb0cf3;0;2;2;FLOAT3;0,0,1;False;4;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;65;-5421.05,-3615.745;Inherit;False;Property;_Layer01BColor;Layer 01 B Color;39;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;67;-5421.05,-3231.745;Inherit;False;Property;_Layer02BColor;Layer 02 B Color;41;0;Create;True;0;0;0;False;0;False;1,0,0,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;49;-2769.221,-1104.216;Inherit;False;236.0591;512.9999;Normal Blend;3;46;47;48;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;94;-5842.717,1166.807;Inherit;False;Property;_Layer03NormalFlatten;Layer 03 Normal Flatten;23;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;95;-5842.717,1390.807;Inherit;False;Property;_Layer04NormalFlatten;Layer 04 Normal Flatten;24;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;60;-4992,-3152;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;24;-2747.769,-391.0769;Inherit;False;2;2;0;FLOAT;20;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;59;-4992,-3536;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;90;-5874.717,1278.807;Inherit;False;Property;_SwitchtoWorldTriplanar;Switch to World Triplanar;23;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;69;-5421.05,-2847.745;Inherit;False;Property;_Layer03BColor;Layer 03 B Color;43;0;Create;True;0;0;0;False;0;False;0,1,0,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;46;-2719.221,-1054.216;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;233;-5602.717,1118.807;Inherit;False;SF_UEFlattenNormal;-1;;318;a744336ecfc2d7743b2d22dfa4eb0cf3;0;2;2;FLOAT3;0,0,1;False;4;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;30;-3488,240;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;68;-5421.05,-3023.745;Inherit;False;Property;_Layer03AColor;Layer 03 A Color;42;0;Create;True;0;0;0;False;0;False;0,1,0,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;234;-5602.717,1342.807;Inherit;False;SF_UEFlattenNormal;-1;;319;a744336ecfc2d7743b2d22dfa4eb0cf3;0;2;2;FLOAT3;0,0,1;False;4;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;61;-4992,-2768;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;56;-3105.301,-2679.025;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;25;-2587.769,-391.0769;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;36;-2635.769,8.92356;Inherit;False;Property;_RoughnessMax;Roughness Max;50;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;45;-3018.039,-2038.938;Inherit;False;508.0591;641;Comment;7;42;41;43;38;44;39;40;Specular Value Lerp;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;47;-2715.162,-905.6813;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;70;-5421.05,-2639.745;Inherit;False;Property;_Layer04AColor;Layer 04 A Color;44;0;Create;True;0;0;0;False;0;False;0,0,1,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;34;-2651.769,88.92288;Inherit;False;True;False;False;False;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;27;-2647.008,-199.2421;Inherit;False;Property;_DistanceBlendContrast;Distance Blend Contrast;27;0;Create;True;0;0;0;False;0;False;4;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;35;-2635.769,-71.07647;Inherit;False;Property;_RoughnessMin;Roughness Min;51;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;71;-5421.05,-2463.745;Inherit;False;Property;_Layer04BColor;Layer 04 B Color;45;0;Create;True;0;0;0;False;0;False;0,0,1,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;21;-978,30;Inherit;False;730;344;Add Normal Texture;3;10;9;8;;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;33;-2411.769,-7.076473;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;48;-2719.221,-750.2164;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;42;-2968.039,-1892.938;Inherit;False;Property;_Layer2Spec;Layer 2 Spec;47;0;Create;True;0;0;0;False;0;False;0.25;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;20;-1458,-226;Inherit;False;438.8999;468.4;Blend Switches;6;12;13;15;19;18;17;;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;62;-4992,-2384;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;26;-2387.008,-263.242;Inherit;False;SF_UECheapContrast;-1;;311;93d366414e6f1cc45b1077ffab365271;0;2;2;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;41;-2968.039,-1988.938;Inherit;False;Property;_Layer1Spec;Layer 1 Spec;46;0;Create;True;0;0;0;False;0;False;0.25;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;57;-3101.242,-2530.49;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;10;-928,176;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;5,5;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;38;-2696.039,-1860.938;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;43;-2968.039,-1796.938;Inherit;False;Property;_Layer3Spec;Layer 3 Spec;48;0;Create;True;0;0;0;False;0;False;0.25;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;18;-1408,-16;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;58;-3105.301,-2375.025;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;19;-1408,112;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;17;-1408,-144;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;87;-724.9771,404.6237;Inherit;False;202;185;AO;1;23;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;44;-2968.039,-1700.938;Inherit;False;Property;_Layer4Spec;Layer 4 Spec;49;0;Create;True;0;0;0;False;0;False;0.25;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;9;-701,142;Inherit;True;Property;_TerrainNormal;Terrain Normal;37;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;-1;fe53404d4a3bea64f910c7c9a40716c3;fe53404d4a3bea64f910c7c9a40716c3;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StaticSwitch;13;-1232,-48;Inherit;False;Property;_BlendOn;Blend On;3;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;39;-2691.98,-1712.403;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;12;-1232,80;Inherit;False;Property;_BlendOn;Blend On;12;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;241;-10645.14,-1605.005;Inherit;True;Property;_TX_Rock_N;TX_Rock_N;57;0;Create;True;0;0;0;False;0;False;-1;abf790eb2fc5a2a4da76c3d8356191fd;abf790eb2fc5a2a4da76c3d8356191fd;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StaticSwitch;15;-1232,-176;Inherit;False;Property;_BlendOn;Blend On;27;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ComponentMaskNode;86;-3503,390;Inherit;False;False;True;False;False;1;0;COLOR;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;23;-674.9771,454.6237;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;40;-2696.039,-1556.938;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;242;-10305.46,-1675.036;Inherit;True;True;True;False;True;1;0;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;243;-10897.27,-1143.336;Inherit;True;Property;_Texture4;Texture 4;57;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;abf790eb2fc5a2a4da76c3d8356191fd;None;True;bump;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SamplerNode;244;-10833.55,-936.9319;Inherit;True;Property;_TX_Flowmap_03_N;TX_Flowmap_03_N;58;0;Create;True;0;0;0;False;0;False;-1;19d9b8e13eb43134ebfd13159334e9c9;19d9b8e13eb43134ebfd13159334e9c9;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;240;-9867.758,-1552.963;Inherit;True;True;True;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;245;-10847.86,-1345.132;Inherit;True;Property;_TextureSample10;Texture Sample 10;59;0;Create;True;0;0;0;False;0;False;-1;abf790eb2fc5a2a4da76c3d8356191fd;abf790eb2fc5a2a4da76c3d8356191fd;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;8;-400,80;Inherit;True;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.OneMinusNode;14;-400,-48;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;239;-10177.56,-1493.262;Inherit;True;Property;_TextureSample9;Texture Sample 9;65;0;Create;True;0;0;0;False;0;False;-1;abf790eb2fc5a2a4da76c3d8356191fd;abf790eb2fc5a2a4da76c3d8356191fd;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthNormals;0;6;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=DepthNormals;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;-114.2203,-154.3999;Float;False;True;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;SH_Env_Tileable;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;18;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;38;Workflow;0;637920277598208202;Surface;0;0;  Refraction Model;0;0;  Blend;0;0;Two Sided;1;0;Fragment Normal Space,InvertActionOnDeselection;0;0;Transmission;0;0;  Transmission Shadow;0.5,False,-1;0;Translucency;0;0;  Translucency Strength;1,False,-1;0;  Normal Distortion;0.5,False,-1;0;  Scattering;2,False,-1;0;  Direct;0.9,False,-1;0;  Ambient;0.1,False,-1;0;  Shadow;0.5,False,-1;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;GPU Instancing;1;0;LOD CrossFade;1;0;Built-in Fog;1;0;_FinalColorxAlpha;0;0;Meta Pass;1;0;Override Baked GI;0;0;Extra Pre Pass;0;0;DOTS Instancing;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,-1;0;  Type;0;0;  Tess;16,False,-1;0;  Min;10,False,-1;0;  Max;25,False,-1;0;  Edge Length;16,False,-1;0;  Max Displacement;25,False,-1;0;Write Depth;0;0;  Early Z;0;0;Vertex Position,InvertActionOnDeselection;1;0;0;8;False;True;True;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;GBuffer;0;7;GBuffer;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalGBuffer;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;151;0;152;0
WireConnection;150;4;151;0
WireConnection;149;0;203;0
WireConnection;149;1;150;0
WireConnection;144;0;146;0
WireConnection;144;1;145;0
WireConnection;148;0;149;0
WireConnection;143;0;144;0
WireConnection;147;0;148;0
WireConnection;142;0;143;0
WireConnection;140;0;142;0
WireConnection;140;1;147;0
WireConnection;139;0;140;0
WireConnection;132;0;133;0
WireConnection;138;0;139;0
WireConnection;131;4;132;0
WireConnection;136;0;138;0
WireConnection;136;1;137;0
WireConnection;134;0;131;0
WireConnection;134;1;136;0
WireConnection;134;2;135;0
WireConnection;124;47;125;0
WireConnection;124;14;134;0
WireConnection;124;11;126;0
WireConnection;124;90;129;0
WireConnection;124;21;131;0
WireConnection;124;25;128;0
WireConnection;124;27;127;0
WireConnection;115;0;131;0
WireConnection;115;1;124;62
WireConnection;115;2;120;0
WireConnection;198;0;197;0
WireConnection;198;1;204;0
WireConnection;114;0;131;0
WireConnection;114;1;124;62
WireConnection;114;2;118;0
WireConnection;200;0;198;0
WireConnection;200;1;205;0
WireConnection;106;0;115;0
WireConnection;106;1;110;0
WireConnection;177;0;109;0
WireConnection;177;1;225;0
WireConnection;178;0;110;0
WireConnection;178;1;225;0
WireConnection;116;0;131;0
WireConnection;116;1;124;62
WireConnection;116;2;121;0
WireConnection;105;0;114;0
WireConnection;105;1;109;0
WireConnection;158;0;169;0
WireConnection;158;1;105;0
WireConnection;173;36;169;0
WireConnection;173;45;177;0
WireConnection;173;9;164;0
WireConnection;170;36;174;0
WireConnection;170;45;178;0
WireConnection;170;9;166;0
WireConnection;196;0;200;0
WireConnection;159;0;174;0
WireConnection;159;1;106;0
WireConnection;117;0;131;0
WireConnection;117;1;124;62
WireConnection;117;2;122;0
WireConnection;179;0;111;0
WireConnection;179;1;225;0
WireConnection;107;0;116;0
WireConnection;107;1;111;0
WireConnection;171;36;175;0
WireConnection;171;45;179;0
WireConnection;171;9;167;0
WireConnection;108;0;117;0
WireConnection;108;1;112;0
WireConnection;180;0;112;0
WireConnection;180;1;225;0
WireConnection;82;1;158;0
WireConnection;82;0;173;2
WireConnection;193;0;196;0
WireConnection;160;0;175;0
WireConnection;160;1;107;0
WireConnection;83;1;159;0
WireConnection;83;0;170;2
WireConnection;194;0;196;1
WireConnection;76;0;83;0
WireConnection;84;1;160;0
WireConnection;84;0;171;2
WireConnection;172;36;176;0
WireConnection;172;45;180;0
WireConnection;172;9;168;0
WireConnection;190;0;193;0
WireConnection;161;0;176;0
WireConnection;161;1;108;0
WireConnection;75;0;82;0
WireConnection;214;0;109;0
WireConnection;184;0;190;0
WireConnection;184;1;187;0
WireConnection;195;0;196;2
WireConnection;78;0;84;0
WireConnection;85;1;161;0
WireConnection;85;0;172;2
WireConnection;74;0;75;0
WireConnection;191;0;194;0
WireConnection;77;0;76;0
WireConnection;215;0;110;0
WireConnection;213;30;219;0
WireConnection;213;71;214;0
WireConnection;213;75;223;0
WireConnection;79;0;78;0
WireConnection;80;0;85;0
WireConnection;210;30;220;0
WireConnection;210;71;215;0
WireConnection;210;75;223;0
WireConnection;185;0;191;0
WireConnection;185;1;188;0
WireConnection;50;1;184;0
WireConnection;50;2;74;0
WireConnection;50;3;77;0
WireConnection;50;4;53;0
WireConnection;216;0;111;0
WireConnection;102;0;220;0
WireConnection;102;1;106;0
WireConnection;101;0;219;0
WireConnection;101;1;105;0
WireConnection;192;0;195;0
WireConnection;81;0;80;0
WireConnection;103;0;221;0
WireConnection;103;1;107;0
WireConnection;186;0;192;0
WireConnection;186;1;189;0
WireConnection;28;0;82;0
WireConnection;28;1;83;0
WireConnection;28;2;50;0
WireConnection;88;1;102;0
WireConnection;88;0;210;0
WireConnection;217;0;112;0
WireConnection;211;30;221;0
WireConnection;211;71;216;0
WireConnection;211;75;223;0
WireConnection;51;1;185;0
WireConnection;51;2;50;6
WireConnection;51;3;79;0
WireConnection;51;4;54;0
WireConnection;91;1;101;0
WireConnection;91;0;213;0
WireConnection;212;30;222;0
WireConnection;212;71;217;0
WireConnection;212;75;223;0
WireConnection;89;1;103;0
WireConnection;89;0;211;0
WireConnection;29;0;28;0
WireConnection;29;1;84;0
WireConnection;29;2;51;0
WireConnection;52;1;186;0
WireConnection;52;2;51;6
WireConnection;52;3;81;0
WireConnection;52;4;55;0
WireConnection;231;2;91;0
WireConnection;231;4;92;0
WireConnection;104;0;222;0
WireConnection;104;1;108;0
WireConnection;232;2;88;0
WireConnection;232;4;93;0
WireConnection;60;0;66;0
WireConnection;60;1;67;0
WireConnection;60;2;77;0
WireConnection;59;0;64;0
WireConnection;59;1;65;0
WireConnection;59;2;74;0
WireConnection;90;1;104;0
WireConnection;90;0;212;0
WireConnection;46;0;231;0
WireConnection;46;1;232;0
WireConnection;46;2;50;0
WireConnection;233;2;89;0
WireConnection;233;4;94;0
WireConnection;30;0;29;0
WireConnection;30;1;85;0
WireConnection;30;2;52;0
WireConnection;234;2;90;0
WireConnection;234;4;95;0
WireConnection;61;0;68;0
WireConnection;61;1;69;0
WireConnection;61;2;79;0
WireConnection;56;0;59;0
WireConnection;56;1;60;0
WireConnection;56;2;50;0
WireConnection;25;0;24;0
WireConnection;47;0;46;0
WireConnection;47;1;233;0
WireConnection;47;2;51;0
WireConnection;34;0;30;0
WireConnection;33;0;35;0
WireConnection;33;1;36;0
WireConnection;33;2;34;0
WireConnection;48;0;47;0
WireConnection;48;1;234;0
WireConnection;48;2;52;0
WireConnection;62;0;70;0
WireConnection;62;1;71;0
WireConnection;62;2;81;0
WireConnection;26;2;25;0
WireConnection;26;1;27;0
WireConnection;57;0;56;0
WireConnection;57;1;61;0
WireConnection;57;2;51;0
WireConnection;38;0;41;0
WireConnection;38;1;42;0
WireConnection;38;2;50;0
WireConnection;18;0;33;0
WireConnection;18;1;74;0
WireConnection;18;2;26;0
WireConnection;58;0;57;0
WireConnection;58;1;62;0
WireConnection;58;2;52;0
WireConnection;19;0;48;0
WireConnection;19;1;91;0
WireConnection;19;2;26;0
WireConnection;17;0;58;0
WireConnection;17;1;62;0
WireConnection;17;2;26;0
WireConnection;9;1;10;0
WireConnection;13;1;33;0
WireConnection;13;0;18;0
WireConnection;39;0;38;0
WireConnection;39;1;43;0
WireConnection;39;2;51;0
WireConnection;12;1;48;0
WireConnection;12;0;19;0
WireConnection;241;0;243;0
WireConnection;15;1;58;0
WireConnection;15;0;17;0
WireConnection;86;0;30;0
WireConnection;23;0;86;0
WireConnection;40;0;39;0
WireConnection;40;1;44;0
WireConnection;40;2;52;0
WireConnection;242;0;241;0
WireConnection;240;0;239;0
WireConnection;8;0;12;0
WireConnection;8;1;9;0
WireConnection;14;0;13;0
WireConnection;239;0;219;0
WireConnection;1;0;15;0
WireConnection;1;1;8;0
WireConnection;1;9;40;0
WireConnection;1;4;14;0
ASEEND*/
//CHKSM=5FCE06EEFA6FB0383EC2CDFC868F0CDA8A750958