// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "VFXA/SH_River"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_DistanceDivider("Distance Divider", Float) = 0.001
		_DistancePower("Distance Power", Float) = 1
		_FoamContrast("Foam Contrast", Float) = 2
		_FoamExp("Foam Exp", Float) = 8
		_FoamHigh("Foam High", Float) = 0
		_FoamIntensity("Foam Intensity", Float) = 16
		_FoamLow("Foam Low", Float) = 0
		_Specular("Specular", Float) = 1
		_WaterRoughness("Water Roughness", Float) = 0
		_UseMipBias("Use Mip Bias", Float) = 1
		[HDR]_BaseColor("Base Color", Color) = (0,0,0,0)
		_CPDirection("CP Direction", Vector) = (0,0,0,0)
		[HDR]_FoamColor("Foam Color", Color) = (0,0,0,0)
		_WaterOpacityMaskOffset("Water Opacity Mask Offset", Float) = -24
		_RiverFlowmapDepthBias("River Flowmap Depth Bias", Float) = 0.4
		_RiverFlowmapDepthDensity("River Flowmap Depth Density", Float) = 0.003
		_RiverFlowmapDetectionVelocity("River Flowmap Detection Velocity", Float) = 0.1
		_RiverFlowmapSpeed("River Flowmap Speed", Float) = 0.003
		_RiverFoamScale("River Foam Scale", Float) = 5.12
		_RiverNormalFlatness("River Normal Flatness", Float) = 0.5
		_RiverTimeModulationSize("River Time Modulation Size", Float) = 0.64
		[ASEEnd]_DISTANCE_TO_NEAREST_SURFACE("DISTANCE_TO_NEAREST_SURFACE", Float) = 100
		[HideInInspector][NoScaleOffset]_Texture2("Texture 2", 2D) = "white" {}
		[HideInInspector][NoScaleOffset][Normal]_Texture3("Texture 3", 2D) = "white" {}
		[HideInInspector]_Noise("Noise", 2D) = "white" {}

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

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="Transparent" }
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
			
			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
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
			#define ASE_NEEDS_FRAG_COLOR


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
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _FoamColor;
			float4 _CPDirection;
			float _Specular;
			float _RiverFlowmapDetectionVelocity;
			float _RiverNormalFlatness;
			float _FoamIntensity;
			float _FoamExp;
			float _RiverFlowmapSpeed;
			float _RiverFlowmapDepthBias;
			float _RiverFlowmapDepthDensity;
			float _RiverTimeModulationSize;
			float _RiverFoamScale;
			float _UseMipBias;
			float _FoamHigh;
			float _FoamLow;
			float _DistancePower;
			float _DistanceDivider;
			float _DISTANCE_TO_NEAREST_SURFACE;
			float _FoamContrast;
			float _WaterRoughness;
			float _WaterOpacityMaskOffset;
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
			sampler2D _Texture2;
			sampler2D _Noise;
			sampler2D _Texture3;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_color = v.ase_color;
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

				float temp_output_1_0_g54 = _FoamContrast;
				int temp_output_90_0_g46 = (int)_UseMipBias;
				float2 temp_output_21_0_g46 = (( WorldPosition / _RiverFoamScale )).xy;
				float3 temp_cast_2 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g44 = ( temp_cast_2 * 0.25 );
				float3 temp_output_103_0 = temp_output_3_0_g44;
				float temp_output_11_0_g46 = ( ( tex2D( _Noise, ( float3( (( WorldPosition / _RiverTimeModulationSize )).xy ,  0.0 ) + ( 0.02 * temp_output_103_0 ) ).xy ).r * 0.25 ) + temp_output_103_0 ).x;
				float2 appendResult69 = (float2(( IN.ase_color.b + _RiverFlowmapDepthBias ) , IN.ase_color.b));
				float2 temp_output_64_0 = (cross( IN.ase_color.rgb , _CPDirection.xyz )).xy;
				float2 temp_output_15_0_g46 = ( ( ( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) * _RiverFlowmapSpeed ) * float2( 1,-1 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g46 = ( frac( ( temp_output_11_0_g46 - 0.5 ) ) * temp_output_15_0_g46 );
				float2 temp_output_19_0_g46 = ( temp_output_21_0_g46 + temp_output_16_0_g46 );
				float2 temp_output_51_0_g46 = ( frac( temp_output_19_0_g46 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g47 = temp_output_21_0_g46.x;
				float2 temp_output_6_0_g47 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g47 = ( ddx( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult9_g47 = dot( temp_output_8_0_g47 , temp_output_8_0_g47 );
				float2 temp_output_7_0_g47 = ( ddy( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult10_g47 = dot( temp_output_7_0_g47 , temp_output_7_0_g47 );
				float ifLocalVar11_g47 = 0;
				if( dotResult9_g47 > dotResult10_g47 )
				ifLocalVar11_g47 = dotResult9_g47;
				else if( dotResult9_g47 < dotResult10_g47 )
				ifLocalVar11_g47 = dotResult10_g47;
				float temp_output_26_0_g46 = ( log2( sqrt( ifLocalVar11_g47 ) ) + 1.0 );
				float4 tex2DNode49_g46 = tex2Dlod( _Texture2, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar88_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar88_g46 = tex2D( _Texture2, temp_output_51_0_g46 );
				else
				ifLocalVar88_g46 = tex2DNode49_g46;
				float temp_output_12_0_g46 = frac( temp_output_11_0_g46 );
				float2 temp_output_13_0_g46 = ( temp_output_15_0_g46 * temp_output_12_0_g46 );
				float2 temp_output_22_0_g46 = ( temp_output_13_0_g46 + temp_output_21_0_g46 );
				float4 tex2DNode32_g46 = tex2Dlod( _Texture2, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar83_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar83_g46 = tex2D( _Texture2, temp_output_22_0_g46 );
				else
				ifLocalVar83_g46 = tex2DNode32_g46;
				float temp_output_2_0_g48 = temp_output_12_0_g46;
				float temp_output_3_0_g48 = 1.0;
				float temp_output_4_0_g48 = ( temp_output_2_0_g48 + ( 0.25 * temp_output_3_0_g48 ) );
				float ifLocalVar24_g48 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g48 = temp_output_2_0_g48;
				else
				ifLocalVar24_g48 = temp_output_4_0_g48;
				float temp_output_7_0_g48 = frac( ( ifLocalVar24_g48 / temp_output_3_0_g48 ) );
				float temp_output_8_0_g48 = ( 2.0 * temp_output_7_0_g48 );
				float temp_output_12_0_g48 = floor( temp_output_8_0_g48 );
				float lerpResult13_g48 = lerp( temp_output_8_0_g48 , ( 2.0 * ( 1.0 - temp_output_7_0_g48 ) ) , temp_output_12_0_g48);
				float temp_output_78_0_g46 = lerpResult13_g48;
				float4 lerpResult69_g46 = lerp( ifLocalVar88_g46 , ifLocalVar83_g46 , temp_output_78_0_g46);
				float lerpResult29 = lerp( _FoamLow , _FoamHigh , ( ( ((lerpResult69_g46).rgb).x + 0.5 ) / 2.0 ));
				float lerpResult5_g54 = lerp( ( 0.0 - temp_output_1_0_g54 ) , ( temp_output_1_0_g54 + 1.0 ) , ( ( 1.0 - saturate( pow( ( _DISTANCE_TO_NEAREST_SURFACE / _DistanceDivider ) , _DistancePower ) ) ) * ( pow( lerpResult29 , _FoamExp ) * _FoamIntensity ) ));
				float temp_output_12_0 = saturate( saturate( lerpResult5_g54 ) );
				float4 lerpResult8 = lerp( _BaseColor , _FoamColor , temp_output_12_0);
				
				float4 tex2DNode48_g46 = tex2Dlod( _Texture3, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar89_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar89_g46 = tex2D( _Texture3, temp_output_51_0_g46 );
				else
				ifLocalVar89_g46 = tex2DNode48_g46;
				float4 tex2DNode34_g46 = tex2Dlod( _Texture3, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar84_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar84_g46 = tex2D( _Texture3, temp_output_22_0_g46 );
				else
				ifLocalVar84_g46 = tex2DNode34_g46;
				float4 lerpResult68_g46 = lerp( ifLocalVar89_g46 , ifLocalVar84_g46 , temp_output_78_0_g46);
				float3 temp_cast_10 = (lerpResult68_g46.r).xxx;
				float3 lerpResult1_g56 = lerp( temp_cast_10 , float3(0,0,1) , _RiverNormalFlatness);
				float3 lerpResult38 = lerp( float3(0,0,1) , lerpResult1_g56 , saturate( ( length( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) ) / _RiverFlowmapDetectionVelocity ) ));
				
				float3 temp_cast_11 = (_Specular).xxx;
				
				float lerpResult88 = lerp( -1.0 , 1.0 , saturate( ( IN.ase_color.b - _WaterOpacityMaskOffset ) ));
				float temp_output_6_0_g55 = saturate( lerpResult88 );
				
				float3 Albedo = lerpResult8.rgb;
				float3 Normal = lerpResult38;
				float3 Emission = 0;
				float3 Specular = temp_cast_11;
				float Metallic = 0;
				float Smoothness = ( 1.0 - _WaterRoughness );
				float Occlusion = 1;
				float Alpha = max( temp_output_12_0 , temp_output_6_0_g55 );
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

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_COLOR


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
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _FoamColor;
			float4 _CPDirection;
			float _Specular;
			float _RiverFlowmapDetectionVelocity;
			float _RiverNormalFlatness;
			float _FoamIntensity;
			float _FoamExp;
			float _RiverFlowmapSpeed;
			float _RiverFlowmapDepthBias;
			float _RiverFlowmapDepthDensity;
			float _RiverTimeModulationSize;
			float _RiverFoamScale;
			float _UseMipBias;
			float _FoamHigh;
			float _FoamLow;
			float _DistancePower;
			float _DistanceDivider;
			float _DISTANCE_TO_NEAREST_SURFACE;
			float _FoamContrast;
			float _WaterRoughness;
			float _WaterOpacityMaskOffset;
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
			sampler2D _Texture2;
			sampler2D _Noise;


			
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

				o.ase_color = v.ase_color;
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

				float temp_output_1_0_g54 = _FoamContrast;
				int temp_output_90_0_g46 = (int)_UseMipBias;
				float2 temp_output_21_0_g46 = (( WorldPosition / _RiverFoamScale )).xy;
				float3 temp_cast_2 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g44 = ( temp_cast_2 * 0.25 );
				float3 temp_output_103_0 = temp_output_3_0_g44;
				float temp_output_11_0_g46 = ( ( tex2D( _Noise, ( float3( (( WorldPosition / _RiverTimeModulationSize )).xy ,  0.0 ) + ( 0.02 * temp_output_103_0 ) ).xy ).r * 0.25 ) + temp_output_103_0 ).x;
				float2 appendResult69 = (float2(( IN.ase_color.b + _RiverFlowmapDepthBias ) , IN.ase_color.b));
				float2 temp_output_64_0 = (cross( IN.ase_color.rgb , _CPDirection.xyz )).xy;
				float2 temp_output_15_0_g46 = ( ( ( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) * _RiverFlowmapSpeed ) * float2( 1,-1 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g46 = ( frac( ( temp_output_11_0_g46 - 0.5 ) ) * temp_output_15_0_g46 );
				float2 temp_output_19_0_g46 = ( temp_output_21_0_g46 + temp_output_16_0_g46 );
				float2 temp_output_51_0_g46 = ( frac( temp_output_19_0_g46 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g47 = temp_output_21_0_g46.x;
				float2 temp_output_6_0_g47 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g47 = ( ddx( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult9_g47 = dot( temp_output_8_0_g47 , temp_output_8_0_g47 );
				float2 temp_output_7_0_g47 = ( ddy( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult10_g47 = dot( temp_output_7_0_g47 , temp_output_7_0_g47 );
				float ifLocalVar11_g47 = 0;
				if( dotResult9_g47 > dotResult10_g47 )
				ifLocalVar11_g47 = dotResult9_g47;
				else if( dotResult9_g47 < dotResult10_g47 )
				ifLocalVar11_g47 = dotResult10_g47;
				float temp_output_26_0_g46 = ( log2( sqrt( ifLocalVar11_g47 ) ) + 1.0 );
				float4 tex2DNode49_g46 = tex2Dlod( _Texture2, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar88_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar88_g46 = tex2D( _Texture2, temp_output_51_0_g46 );
				else
				ifLocalVar88_g46 = tex2DNode49_g46;
				float temp_output_12_0_g46 = frac( temp_output_11_0_g46 );
				float2 temp_output_13_0_g46 = ( temp_output_15_0_g46 * temp_output_12_0_g46 );
				float2 temp_output_22_0_g46 = ( temp_output_13_0_g46 + temp_output_21_0_g46 );
				float4 tex2DNode32_g46 = tex2Dlod( _Texture2, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar83_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar83_g46 = tex2D( _Texture2, temp_output_22_0_g46 );
				else
				ifLocalVar83_g46 = tex2DNode32_g46;
				float temp_output_2_0_g48 = temp_output_12_0_g46;
				float temp_output_3_0_g48 = 1.0;
				float temp_output_4_0_g48 = ( temp_output_2_0_g48 + ( 0.25 * temp_output_3_0_g48 ) );
				float ifLocalVar24_g48 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g48 = temp_output_2_0_g48;
				else
				ifLocalVar24_g48 = temp_output_4_0_g48;
				float temp_output_7_0_g48 = frac( ( ifLocalVar24_g48 / temp_output_3_0_g48 ) );
				float temp_output_8_0_g48 = ( 2.0 * temp_output_7_0_g48 );
				float temp_output_12_0_g48 = floor( temp_output_8_0_g48 );
				float lerpResult13_g48 = lerp( temp_output_8_0_g48 , ( 2.0 * ( 1.0 - temp_output_7_0_g48 ) ) , temp_output_12_0_g48);
				float temp_output_78_0_g46 = lerpResult13_g48;
				float4 lerpResult69_g46 = lerp( ifLocalVar88_g46 , ifLocalVar83_g46 , temp_output_78_0_g46);
				float lerpResult29 = lerp( _FoamLow , _FoamHigh , ( ( ((lerpResult69_g46).rgb).x + 0.5 ) / 2.0 ));
				float lerpResult5_g54 = lerp( ( 0.0 - temp_output_1_0_g54 ) , ( temp_output_1_0_g54 + 1.0 ) , ( ( 1.0 - saturate( pow( ( _DISTANCE_TO_NEAREST_SURFACE / _DistanceDivider ) , _DistancePower ) ) ) * ( pow( lerpResult29 , _FoamExp ) * _FoamIntensity ) ));
				float temp_output_12_0 = saturate( saturate( lerpResult5_g54 ) );
				float lerpResult88 = lerp( -1.0 , 1.0 , saturate( ( IN.ase_color.b - _WaterOpacityMaskOffset ) ));
				float temp_output_6_0_g55 = saturate( lerpResult88 );
				
				float Alpha = max( temp_output_12_0 , temp_output_6_0_g55 );
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

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_COLOR


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
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _FoamColor;
			float4 _CPDirection;
			float _Specular;
			float _RiverFlowmapDetectionVelocity;
			float _RiverNormalFlatness;
			float _FoamIntensity;
			float _FoamExp;
			float _RiverFlowmapSpeed;
			float _RiverFlowmapDepthBias;
			float _RiverFlowmapDepthDensity;
			float _RiverTimeModulationSize;
			float _RiverFoamScale;
			float _UseMipBias;
			float _FoamHigh;
			float _FoamLow;
			float _DistancePower;
			float _DistanceDivider;
			float _DISTANCE_TO_NEAREST_SURFACE;
			float _FoamContrast;
			float _WaterRoughness;
			float _WaterOpacityMaskOffset;
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
			sampler2D _Texture2;
			sampler2D _Noise;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_color = v.ase_color;
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

				float temp_output_1_0_g54 = _FoamContrast;
				int temp_output_90_0_g46 = (int)_UseMipBias;
				float2 temp_output_21_0_g46 = (( WorldPosition / _RiverFoamScale )).xy;
				float3 temp_cast_2 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g44 = ( temp_cast_2 * 0.25 );
				float3 temp_output_103_0 = temp_output_3_0_g44;
				float temp_output_11_0_g46 = ( ( tex2D( _Noise, ( float3( (( WorldPosition / _RiverTimeModulationSize )).xy ,  0.0 ) + ( 0.02 * temp_output_103_0 ) ).xy ).r * 0.25 ) + temp_output_103_0 ).x;
				float2 appendResult69 = (float2(( IN.ase_color.b + _RiverFlowmapDepthBias ) , IN.ase_color.b));
				float2 temp_output_64_0 = (cross( IN.ase_color.rgb , _CPDirection.xyz )).xy;
				float2 temp_output_15_0_g46 = ( ( ( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) * _RiverFlowmapSpeed ) * float2( 1,-1 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g46 = ( frac( ( temp_output_11_0_g46 - 0.5 ) ) * temp_output_15_0_g46 );
				float2 temp_output_19_0_g46 = ( temp_output_21_0_g46 + temp_output_16_0_g46 );
				float2 temp_output_51_0_g46 = ( frac( temp_output_19_0_g46 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g47 = temp_output_21_0_g46.x;
				float2 temp_output_6_0_g47 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g47 = ( ddx( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult9_g47 = dot( temp_output_8_0_g47 , temp_output_8_0_g47 );
				float2 temp_output_7_0_g47 = ( ddy( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult10_g47 = dot( temp_output_7_0_g47 , temp_output_7_0_g47 );
				float ifLocalVar11_g47 = 0;
				if( dotResult9_g47 > dotResult10_g47 )
				ifLocalVar11_g47 = dotResult9_g47;
				else if( dotResult9_g47 < dotResult10_g47 )
				ifLocalVar11_g47 = dotResult10_g47;
				float temp_output_26_0_g46 = ( log2( sqrt( ifLocalVar11_g47 ) ) + 1.0 );
				float4 tex2DNode49_g46 = tex2Dlod( _Texture2, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar88_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar88_g46 = tex2D( _Texture2, temp_output_51_0_g46 );
				else
				ifLocalVar88_g46 = tex2DNode49_g46;
				float temp_output_12_0_g46 = frac( temp_output_11_0_g46 );
				float2 temp_output_13_0_g46 = ( temp_output_15_0_g46 * temp_output_12_0_g46 );
				float2 temp_output_22_0_g46 = ( temp_output_13_0_g46 + temp_output_21_0_g46 );
				float4 tex2DNode32_g46 = tex2Dlod( _Texture2, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar83_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar83_g46 = tex2D( _Texture2, temp_output_22_0_g46 );
				else
				ifLocalVar83_g46 = tex2DNode32_g46;
				float temp_output_2_0_g48 = temp_output_12_0_g46;
				float temp_output_3_0_g48 = 1.0;
				float temp_output_4_0_g48 = ( temp_output_2_0_g48 + ( 0.25 * temp_output_3_0_g48 ) );
				float ifLocalVar24_g48 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g48 = temp_output_2_0_g48;
				else
				ifLocalVar24_g48 = temp_output_4_0_g48;
				float temp_output_7_0_g48 = frac( ( ifLocalVar24_g48 / temp_output_3_0_g48 ) );
				float temp_output_8_0_g48 = ( 2.0 * temp_output_7_0_g48 );
				float temp_output_12_0_g48 = floor( temp_output_8_0_g48 );
				float lerpResult13_g48 = lerp( temp_output_8_0_g48 , ( 2.0 * ( 1.0 - temp_output_7_0_g48 ) ) , temp_output_12_0_g48);
				float temp_output_78_0_g46 = lerpResult13_g48;
				float4 lerpResult69_g46 = lerp( ifLocalVar88_g46 , ifLocalVar83_g46 , temp_output_78_0_g46);
				float lerpResult29 = lerp( _FoamLow , _FoamHigh , ( ( ((lerpResult69_g46).rgb).x + 0.5 ) / 2.0 ));
				float lerpResult5_g54 = lerp( ( 0.0 - temp_output_1_0_g54 ) , ( temp_output_1_0_g54 + 1.0 ) , ( ( 1.0 - saturate( pow( ( _DISTANCE_TO_NEAREST_SURFACE / _DistanceDivider ) , _DistancePower ) ) ) * ( pow( lerpResult29 , _FoamExp ) * _FoamIntensity ) ));
				float temp_output_12_0 = saturate( saturate( lerpResult5_g54 ) );
				float lerpResult88 = lerp( -1.0 , 1.0 , saturate( ( IN.ase_color.b - _WaterOpacityMaskOffset ) ));
				float temp_output_6_0_g55 = saturate( lerpResult88 );
				
				float Alpha = max( temp_output_12_0 , temp_output_6_0_g55 );
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
			#define ASE_NEEDS_FRAG_COLOR


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
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _FoamColor;
			float4 _CPDirection;
			float _Specular;
			float _RiverFlowmapDetectionVelocity;
			float _RiverNormalFlatness;
			float _FoamIntensity;
			float _FoamExp;
			float _RiverFlowmapSpeed;
			float _RiverFlowmapDepthBias;
			float _RiverFlowmapDepthDensity;
			float _RiverTimeModulationSize;
			float _RiverFoamScale;
			float _UseMipBias;
			float _FoamHigh;
			float _FoamLow;
			float _DistancePower;
			float _DistanceDivider;
			float _DISTANCE_TO_NEAREST_SURFACE;
			float _FoamContrast;
			float _WaterRoughness;
			float _WaterOpacityMaskOffset;
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
			sampler2D _Texture2;
			sampler2D _Noise;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_color = v.ase_color;
				
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

				float temp_output_1_0_g54 = _FoamContrast;
				int temp_output_90_0_g46 = (int)_UseMipBias;
				float2 temp_output_21_0_g46 = (( WorldPosition / _RiverFoamScale )).xy;
				float3 temp_cast_2 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g44 = ( temp_cast_2 * 0.25 );
				float3 temp_output_103_0 = temp_output_3_0_g44;
				float temp_output_11_0_g46 = ( ( tex2D( _Noise, ( float3( (( WorldPosition / _RiverTimeModulationSize )).xy ,  0.0 ) + ( 0.02 * temp_output_103_0 ) ).xy ).r * 0.25 ) + temp_output_103_0 ).x;
				float2 appendResult69 = (float2(( IN.ase_color.b + _RiverFlowmapDepthBias ) , IN.ase_color.b));
				float2 temp_output_64_0 = (cross( IN.ase_color.rgb , _CPDirection.xyz )).xy;
				float2 temp_output_15_0_g46 = ( ( ( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) * _RiverFlowmapSpeed ) * float2( 1,-1 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g46 = ( frac( ( temp_output_11_0_g46 - 0.5 ) ) * temp_output_15_0_g46 );
				float2 temp_output_19_0_g46 = ( temp_output_21_0_g46 + temp_output_16_0_g46 );
				float2 temp_output_51_0_g46 = ( frac( temp_output_19_0_g46 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g47 = temp_output_21_0_g46.x;
				float2 temp_output_6_0_g47 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g47 = ( ddx( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult9_g47 = dot( temp_output_8_0_g47 , temp_output_8_0_g47 );
				float2 temp_output_7_0_g47 = ( ddy( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult10_g47 = dot( temp_output_7_0_g47 , temp_output_7_0_g47 );
				float ifLocalVar11_g47 = 0;
				if( dotResult9_g47 > dotResult10_g47 )
				ifLocalVar11_g47 = dotResult9_g47;
				else if( dotResult9_g47 < dotResult10_g47 )
				ifLocalVar11_g47 = dotResult10_g47;
				float temp_output_26_0_g46 = ( log2( sqrt( ifLocalVar11_g47 ) ) + 1.0 );
				float4 tex2DNode49_g46 = tex2Dlod( _Texture2, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar88_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar88_g46 = tex2D( _Texture2, temp_output_51_0_g46 );
				else
				ifLocalVar88_g46 = tex2DNode49_g46;
				float temp_output_12_0_g46 = frac( temp_output_11_0_g46 );
				float2 temp_output_13_0_g46 = ( temp_output_15_0_g46 * temp_output_12_0_g46 );
				float2 temp_output_22_0_g46 = ( temp_output_13_0_g46 + temp_output_21_0_g46 );
				float4 tex2DNode32_g46 = tex2Dlod( _Texture2, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar83_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar83_g46 = tex2D( _Texture2, temp_output_22_0_g46 );
				else
				ifLocalVar83_g46 = tex2DNode32_g46;
				float temp_output_2_0_g48 = temp_output_12_0_g46;
				float temp_output_3_0_g48 = 1.0;
				float temp_output_4_0_g48 = ( temp_output_2_0_g48 + ( 0.25 * temp_output_3_0_g48 ) );
				float ifLocalVar24_g48 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g48 = temp_output_2_0_g48;
				else
				ifLocalVar24_g48 = temp_output_4_0_g48;
				float temp_output_7_0_g48 = frac( ( ifLocalVar24_g48 / temp_output_3_0_g48 ) );
				float temp_output_8_0_g48 = ( 2.0 * temp_output_7_0_g48 );
				float temp_output_12_0_g48 = floor( temp_output_8_0_g48 );
				float lerpResult13_g48 = lerp( temp_output_8_0_g48 , ( 2.0 * ( 1.0 - temp_output_7_0_g48 ) ) , temp_output_12_0_g48);
				float temp_output_78_0_g46 = lerpResult13_g48;
				float4 lerpResult69_g46 = lerp( ifLocalVar88_g46 , ifLocalVar83_g46 , temp_output_78_0_g46);
				float lerpResult29 = lerp( _FoamLow , _FoamHigh , ( ( ((lerpResult69_g46).rgb).x + 0.5 ) / 2.0 ));
				float lerpResult5_g54 = lerp( ( 0.0 - temp_output_1_0_g54 ) , ( temp_output_1_0_g54 + 1.0 ) , ( ( 1.0 - saturate( pow( ( _DISTANCE_TO_NEAREST_SURFACE / _DistanceDivider ) , _DistancePower ) ) ) * ( pow( lerpResult29 , _FoamExp ) * _FoamIntensity ) ));
				float temp_output_12_0 = saturate( saturate( lerpResult5_g54 ) );
				float4 lerpResult8 = lerp( _BaseColor , _FoamColor , temp_output_12_0);
				
				float lerpResult88 = lerp( -1.0 , 1.0 , saturate( ( IN.ase_color.b - _WaterOpacityMaskOffset ) ));
				float temp_output_6_0_g55 = saturate( lerpResult88 );
				
				
				float3 Albedo = lerpResult8.rgb;
				float3 Emission = 0;
				float Alpha = max( temp_output_12_0 , temp_output_6_0_g55 );
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

			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
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
			#define ASE_NEEDS_FRAG_COLOR


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
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _FoamColor;
			float4 _CPDirection;
			float _Specular;
			float _RiverFlowmapDetectionVelocity;
			float _RiverNormalFlatness;
			float _FoamIntensity;
			float _FoamExp;
			float _RiverFlowmapSpeed;
			float _RiverFlowmapDepthBias;
			float _RiverFlowmapDepthDensity;
			float _RiverTimeModulationSize;
			float _RiverFoamScale;
			float _UseMipBias;
			float _FoamHigh;
			float _FoamLow;
			float _DistancePower;
			float _DistanceDivider;
			float _DISTANCE_TO_NEAREST_SURFACE;
			float _FoamContrast;
			float _WaterRoughness;
			float _WaterOpacityMaskOffset;
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
			sampler2D _Texture2;
			sampler2D _Noise;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				o.ase_color = v.ase_color;
				
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

				float temp_output_1_0_g54 = _FoamContrast;
				int temp_output_90_0_g46 = (int)_UseMipBias;
				float2 temp_output_21_0_g46 = (( WorldPosition / _RiverFoamScale )).xy;
				float3 temp_cast_2 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g44 = ( temp_cast_2 * 0.25 );
				float3 temp_output_103_0 = temp_output_3_0_g44;
				float temp_output_11_0_g46 = ( ( tex2D( _Noise, ( float3( (( WorldPosition / _RiverTimeModulationSize )).xy ,  0.0 ) + ( 0.02 * temp_output_103_0 ) ).xy ).r * 0.25 ) + temp_output_103_0 ).x;
				float2 appendResult69 = (float2(( IN.ase_color.b + _RiverFlowmapDepthBias ) , IN.ase_color.b));
				float2 temp_output_64_0 = (cross( IN.ase_color.rgb , _CPDirection.xyz )).xy;
				float2 temp_output_15_0_g46 = ( ( ( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) * _RiverFlowmapSpeed ) * float2( 1,-1 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g46 = ( frac( ( temp_output_11_0_g46 - 0.5 ) ) * temp_output_15_0_g46 );
				float2 temp_output_19_0_g46 = ( temp_output_21_0_g46 + temp_output_16_0_g46 );
				float2 temp_output_51_0_g46 = ( frac( temp_output_19_0_g46 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g47 = temp_output_21_0_g46.x;
				float2 temp_output_6_0_g47 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g47 = ( ddx( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult9_g47 = dot( temp_output_8_0_g47 , temp_output_8_0_g47 );
				float2 temp_output_7_0_g47 = ( ddy( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult10_g47 = dot( temp_output_7_0_g47 , temp_output_7_0_g47 );
				float ifLocalVar11_g47 = 0;
				if( dotResult9_g47 > dotResult10_g47 )
				ifLocalVar11_g47 = dotResult9_g47;
				else if( dotResult9_g47 < dotResult10_g47 )
				ifLocalVar11_g47 = dotResult10_g47;
				float temp_output_26_0_g46 = ( log2( sqrt( ifLocalVar11_g47 ) ) + 1.0 );
				float4 tex2DNode49_g46 = tex2Dlod( _Texture2, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar88_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar88_g46 = tex2D( _Texture2, temp_output_51_0_g46 );
				else
				ifLocalVar88_g46 = tex2DNode49_g46;
				float temp_output_12_0_g46 = frac( temp_output_11_0_g46 );
				float2 temp_output_13_0_g46 = ( temp_output_15_0_g46 * temp_output_12_0_g46 );
				float2 temp_output_22_0_g46 = ( temp_output_13_0_g46 + temp_output_21_0_g46 );
				float4 tex2DNode32_g46 = tex2Dlod( _Texture2, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar83_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar83_g46 = tex2D( _Texture2, temp_output_22_0_g46 );
				else
				ifLocalVar83_g46 = tex2DNode32_g46;
				float temp_output_2_0_g48 = temp_output_12_0_g46;
				float temp_output_3_0_g48 = 1.0;
				float temp_output_4_0_g48 = ( temp_output_2_0_g48 + ( 0.25 * temp_output_3_0_g48 ) );
				float ifLocalVar24_g48 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g48 = temp_output_2_0_g48;
				else
				ifLocalVar24_g48 = temp_output_4_0_g48;
				float temp_output_7_0_g48 = frac( ( ifLocalVar24_g48 / temp_output_3_0_g48 ) );
				float temp_output_8_0_g48 = ( 2.0 * temp_output_7_0_g48 );
				float temp_output_12_0_g48 = floor( temp_output_8_0_g48 );
				float lerpResult13_g48 = lerp( temp_output_8_0_g48 , ( 2.0 * ( 1.0 - temp_output_7_0_g48 ) ) , temp_output_12_0_g48);
				float temp_output_78_0_g46 = lerpResult13_g48;
				float4 lerpResult69_g46 = lerp( ifLocalVar88_g46 , ifLocalVar83_g46 , temp_output_78_0_g46);
				float lerpResult29 = lerp( _FoamLow , _FoamHigh , ( ( ((lerpResult69_g46).rgb).x + 0.5 ) / 2.0 ));
				float lerpResult5_g54 = lerp( ( 0.0 - temp_output_1_0_g54 ) , ( temp_output_1_0_g54 + 1.0 ) , ( ( 1.0 - saturate( pow( ( _DISTANCE_TO_NEAREST_SURFACE / _DistanceDivider ) , _DistancePower ) ) ) * ( pow( lerpResult29 , _FoamExp ) * _FoamIntensity ) ));
				float temp_output_12_0 = saturate( saturate( lerpResult5_g54 ) );
				float4 lerpResult8 = lerp( _BaseColor , _FoamColor , temp_output_12_0);
				
				float lerpResult88 = lerp( -1.0 , 1.0 , saturate( ( IN.ase_color.b - _WaterOpacityMaskOffset ) ));
				float temp_output_6_0_g55 = saturate( lerpResult88 );
				
				
				float3 Albedo = lerpResult8.rgb;
				float Alpha = max( temp_output_12_0 , temp_output_6_0_g55 );
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

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_COLOR


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
				float3 worldNormal : TEXCOORD2;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _FoamColor;
			float4 _CPDirection;
			float _Specular;
			float _RiverFlowmapDetectionVelocity;
			float _RiverNormalFlatness;
			float _FoamIntensity;
			float _FoamExp;
			float _RiverFlowmapSpeed;
			float _RiverFlowmapDepthBias;
			float _RiverFlowmapDepthDensity;
			float _RiverTimeModulationSize;
			float _RiverFoamScale;
			float _UseMipBias;
			float _FoamHigh;
			float _FoamLow;
			float _DistancePower;
			float _DistanceDivider;
			float _DISTANCE_TO_NEAREST_SURFACE;
			float _FoamContrast;
			float _WaterRoughness;
			float _WaterOpacityMaskOffset;
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
			sampler2D _Texture2;
			sampler2D _Noise;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_color = v.ase_color;
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

				float temp_output_1_0_g54 = _FoamContrast;
				int temp_output_90_0_g46 = (int)_UseMipBias;
				float2 temp_output_21_0_g46 = (( WorldPosition / _RiverFoamScale )).xy;
				float3 temp_cast_2 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g44 = ( temp_cast_2 * 0.25 );
				float3 temp_output_103_0 = temp_output_3_0_g44;
				float temp_output_11_0_g46 = ( ( tex2D( _Noise, ( float3( (( WorldPosition / _RiverTimeModulationSize )).xy ,  0.0 ) + ( 0.02 * temp_output_103_0 ) ).xy ).r * 0.25 ) + temp_output_103_0 ).x;
				float2 appendResult69 = (float2(( IN.ase_color.b + _RiverFlowmapDepthBias ) , IN.ase_color.b));
				float2 temp_output_64_0 = (cross( IN.ase_color.rgb , _CPDirection.xyz )).xy;
				float2 temp_output_15_0_g46 = ( ( ( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) * _RiverFlowmapSpeed ) * float2( 1,-1 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g46 = ( frac( ( temp_output_11_0_g46 - 0.5 ) ) * temp_output_15_0_g46 );
				float2 temp_output_19_0_g46 = ( temp_output_21_0_g46 + temp_output_16_0_g46 );
				float2 temp_output_51_0_g46 = ( frac( temp_output_19_0_g46 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g47 = temp_output_21_0_g46.x;
				float2 temp_output_6_0_g47 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g47 = ( ddx( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult9_g47 = dot( temp_output_8_0_g47 , temp_output_8_0_g47 );
				float2 temp_output_7_0_g47 = ( ddy( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult10_g47 = dot( temp_output_7_0_g47 , temp_output_7_0_g47 );
				float ifLocalVar11_g47 = 0;
				if( dotResult9_g47 > dotResult10_g47 )
				ifLocalVar11_g47 = dotResult9_g47;
				else if( dotResult9_g47 < dotResult10_g47 )
				ifLocalVar11_g47 = dotResult10_g47;
				float temp_output_26_0_g46 = ( log2( sqrt( ifLocalVar11_g47 ) ) + 1.0 );
				float4 tex2DNode49_g46 = tex2Dlod( _Texture2, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar88_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar88_g46 = tex2D( _Texture2, temp_output_51_0_g46 );
				else
				ifLocalVar88_g46 = tex2DNode49_g46;
				float temp_output_12_0_g46 = frac( temp_output_11_0_g46 );
				float2 temp_output_13_0_g46 = ( temp_output_15_0_g46 * temp_output_12_0_g46 );
				float2 temp_output_22_0_g46 = ( temp_output_13_0_g46 + temp_output_21_0_g46 );
				float4 tex2DNode32_g46 = tex2Dlod( _Texture2, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar83_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar83_g46 = tex2D( _Texture2, temp_output_22_0_g46 );
				else
				ifLocalVar83_g46 = tex2DNode32_g46;
				float temp_output_2_0_g48 = temp_output_12_0_g46;
				float temp_output_3_0_g48 = 1.0;
				float temp_output_4_0_g48 = ( temp_output_2_0_g48 + ( 0.25 * temp_output_3_0_g48 ) );
				float ifLocalVar24_g48 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g48 = temp_output_2_0_g48;
				else
				ifLocalVar24_g48 = temp_output_4_0_g48;
				float temp_output_7_0_g48 = frac( ( ifLocalVar24_g48 / temp_output_3_0_g48 ) );
				float temp_output_8_0_g48 = ( 2.0 * temp_output_7_0_g48 );
				float temp_output_12_0_g48 = floor( temp_output_8_0_g48 );
				float lerpResult13_g48 = lerp( temp_output_8_0_g48 , ( 2.0 * ( 1.0 - temp_output_7_0_g48 ) ) , temp_output_12_0_g48);
				float temp_output_78_0_g46 = lerpResult13_g48;
				float4 lerpResult69_g46 = lerp( ifLocalVar88_g46 , ifLocalVar83_g46 , temp_output_78_0_g46);
				float lerpResult29 = lerp( _FoamLow , _FoamHigh , ( ( ((lerpResult69_g46).rgb).x + 0.5 ) / 2.0 ));
				float lerpResult5_g54 = lerp( ( 0.0 - temp_output_1_0_g54 ) , ( temp_output_1_0_g54 + 1.0 ) , ( ( 1.0 - saturate( pow( ( _DISTANCE_TO_NEAREST_SURFACE / _DistanceDivider ) , _DistancePower ) ) ) * ( pow( lerpResult29 , _FoamExp ) * _FoamIntensity ) ));
				float temp_output_12_0 = saturate( saturate( lerpResult5_g54 ) );
				float lerpResult88 = lerp( -1.0 , 1.0 , saturate( ( IN.ase_color.b - _WaterOpacityMaskOffset ) ));
				float temp_output_6_0_g55 = saturate( lerpResult88 );
				
				float Alpha = max( temp_output_12_0 , temp_output_6_0_g55 );
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
			
			Blend SrcAlpha OneMinusSrcAlpha, One OneMinusSrcAlpha
			ZWrite Off
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
			#define ASE_NEEDS_FRAG_COLOR


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
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _FoamColor;
			float4 _CPDirection;
			float _Specular;
			float _RiverFlowmapDetectionVelocity;
			float _RiverNormalFlatness;
			float _FoamIntensity;
			float _FoamExp;
			float _RiverFlowmapSpeed;
			float _RiverFlowmapDepthBias;
			float _RiverFlowmapDepthDensity;
			float _RiverTimeModulationSize;
			float _RiverFoamScale;
			float _UseMipBias;
			float _FoamHigh;
			float _FoamLow;
			float _DistancePower;
			float _DistanceDivider;
			float _DISTANCE_TO_NEAREST_SURFACE;
			float _FoamContrast;
			float _WaterRoughness;
			float _WaterOpacityMaskOffset;
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
			sampler2D _Texture2;
			sampler2D _Noise;
			sampler2D _Texture3;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_color = v.ase_color;
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

				float temp_output_1_0_g54 = _FoamContrast;
				int temp_output_90_0_g46 = (int)_UseMipBias;
				float2 temp_output_21_0_g46 = (( WorldPosition / _RiverFoamScale )).xy;
				float3 temp_cast_2 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g44 = ( temp_cast_2 * 0.25 );
				float3 temp_output_103_0 = temp_output_3_0_g44;
				float temp_output_11_0_g46 = ( ( tex2D( _Noise, ( float3( (( WorldPosition / _RiverTimeModulationSize )).xy ,  0.0 ) + ( 0.02 * temp_output_103_0 ) ).xy ).r * 0.25 ) + temp_output_103_0 ).x;
				float2 appendResult69 = (float2(( IN.ase_color.b + _RiverFlowmapDepthBias ) , IN.ase_color.b));
				float2 temp_output_64_0 = (cross( IN.ase_color.rgb , _CPDirection.xyz )).xy;
				float2 temp_output_15_0_g46 = ( ( ( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) * _RiverFlowmapSpeed ) * float2( 1,-1 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g46 = ( frac( ( temp_output_11_0_g46 - 0.5 ) ) * temp_output_15_0_g46 );
				float2 temp_output_19_0_g46 = ( temp_output_21_0_g46 + temp_output_16_0_g46 );
				float2 temp_output_51_0_g46 = ( frac( temp_output_19_0_g46 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g47 = temp_output_21_0_g46.x;
				float2 temp_output_6_0_g47 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g47 = ( ddx( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult9_g47 = dot( temp_output_8_0_g47 , temp_output_8_0_g47 );
				float2 temp_output_7_0_g47 = ( ddy( temp_output_2_0_g47 ) * temp_output_6_0_g47 );
				float dotResult10_g47 = dot( temp_output_7_0_g47 , temp_output_7_0_g47 );
				float ifLocalVar11_g47 = 0;
				if( dotResult9_g47 > dotResult10_g47 )
				ifLocalVar11_g47 = dotResult9_g47;
				else if( dotResult9_g47 < dotResult10_g47 )
				ifLocalVar11_g47 = dotResult10_g47;
				float temp_output_26_0_g46 = ( log2( sqrt( ifLocalVar11_g47 ) ) + 1.0 );
				float4 tex2DNode49_g46 = tex2Dlod( _Texture2, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar88_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar88_g46 = tex2D( _Texture2, temp_output_51_0_g46 );
				else
				ifLocalVar88_g46 = tex2DNode49_g46;
				float temp_output_12_0_g46 = frac( temp_output_11_0_g46 );
				float2 temp_output_13_0_g46 = ( temp_output_15_0_g46 * temp_output_12_0_g46 );
				float2 temp_output_22_0_g46 = ( temp_output_13_0_g46 + temp_output_21_0_g46 );
				float4 tex2DNode32_g46 = tex2Dlod( _Texture2, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar83_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar83_g46 = tex2D( _Texture2, temp_output_22_0_g46 );
				else
				ifLocalVar83_g46 = tex2DNode32_g46;
				float temp_output_2_0_g48 = temp_output_12_0_g46;
				float temp_output_3_0_g48 = 1.0;
				float temp_output_4_0_g48 = ( temp_output_2_0_g48 + ( 0.25 * temp_output_3_0_g48 ) );
				float ifLocalVar24_g48 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g48 = temp_output_2_0_g48;
				else
				ifLocalVar24_g48 = temp_output_4_0_g48;
				float temp_output_7_0_g48 = frac( ( ifLocalVar24_g48 / temp_output_3_0_g48 ) );
				float temp_output_8_0_g48 = ( 2.0 * temp_output_7_0_g48 );
				float temp_output_12_0_g48 = floor( temp_output_8_0_g48 );
				float lerpResult13_g48 = lerp( temp_output_8_0_g48 , ( 2.0 * ( 1.0 - temp_output_7_0_g48 ) ) , temp_output_12_0_g48);
				float temp_output_78_0_g46 = lerpResult13_g48;
				float4 lerpResult69_g46 = lerp( ifLocalVar88_g46 , ifLocalVar83_g46 , temp_output_78_0_g46);
				float lerpResult29 = lerp( _FoamLow , _FoamHigh , ( ( ((lerpResult69_g46).rgb).x + 0.5 ) / 2.0 ));
				float lerpResult5_g54 = lerp( ( 0.0 - temp_output_1_0_g54 ) , ( temp_output_1_0_g54 + 1.0 ) , ( ( 1.0 - saturate( pow( ( _DISTANCE_TO_NEAREST_SURFACE / _DistanceDivider ) , _DistancePower ) ) ) * ( pow( lerpResult29 , _FoamExp ) * _FoamIntensity ) ));
				float temp_output_12_0 = saturate( saturate( lerpResult5_g54 ) );
				float4 lerpResult8 = lerp( _BaseColor , _FoamColor , temp_output_12_0);
				
				float4 tex2DNode48_g46 = tex2Dlod( _Texture3, float4( temp_output_51_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar89_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar89_g46 = tex2D( _Texture3, temp_output_51_0_g46 );
				else
				ifLocalVar89_g46 = tex2DNode48_g46;
				float4 tex2DNode34_g46 = tex2Dlod( _Texture3, float4( temp_output_22_0_g46, 0, temp_output_26_0_g46) );
				float4 ifLocalVar84_g46 = 0;
				if( temp_output_90_0_g46 == 0.0 )
				ifLocalVar84_g46 = tex2D( _Texture3, temp_output_22_0_g46 );
				else
				ifLocalVar84_g46 = tex2DNode34_g46;
				float4 lerpResult68_g46 = lerp( ifLocalVar89_g46 , ifLocalVar84_g46 , temp_output_78_0_g46);
				float3 temp_cast_10 = (lerpResult68_g46.r).xxx;
				float3 lerpResult1_g56 = lerp( temp_cast_10 , float3(0,0,1) , _RiverNormalFlatness);
				float3 lerpResult38 = lerp( float3(0,0,1) , lerpResult1_g56 , saturate( ( length( ( exp( ( _RiverFlowmapDepthDensity * appendResult69 ) ).x * temp_output_64_0 ) ) / _RiverFlowmapDetectionVelocity ) ));
				
				float3 temp_cast_11 = (_Specular).xxx;
				
				float lerpResult88 = lerp( -1.0 , 1.0 , saturate( ( IN.ase_color.b - _WaterOpacityMaskOffset ) ));
				float temp_output_6_0_g55 = saturate( lerpResult88 );
				
				float3 Albedo = lerpResult8.rgb;
				float3 Normal = lerpResult38;
				float3 Emission = 0;
				float3 Specular = temp_cast_11;
				float Metallic = 0;
				float Smoothness = ( 1.0 - _WaterRoughness );
				float Occlusion = 1;
				float Alpha = max( temp_output_12_0 , temp_output_6_0_g55 );
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
300;73;1058;567;2812.611;-814.8109;2.298561;True;False
Node;AmplifyShaderEditor.CommentaryNode;75;-5390.02,398;Inherit;False;1504.02;582;Comment;13;74;71;73;69;68;67;70;66;64;62;61;63;108;Flow Vector;1,1,1,1;0;0
Node;AmplifyShaderEditor.VertexColorNode;71;-5328,448;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;74;-5328,864;Inherit;False;Property;_RiverFlowmapDepthBias;River Flowmap Depth Bias;16;0;Create;True;0;0;0;False;0;False;0.4;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;106;-4530,1342;Inherit;False;1722;345;Comment;10;105;101;102;103;99;100;98;97;96;94;Break Up Time;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;73;-5006.02,734.3624;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;69;-4816,704;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;68;-4946.021,602.5895;Inherit;False;Property;_RiverFlowmapDepthDensity;River Flowmap Depth Density;17;0;Create;True;0;0;0;False;0;False;0.003;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;105;-4480,1552;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;102;-4144,1472;Inherit;False;Constant;_Float1;Float 1;28;0;Create;True;0;0;0;False;0;False;0.02;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;67;-4656,640;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector4Node;108;-5328,624;Inherit;False;Property;_CPDirection;CP Direction;12;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;103;-4288,1552;Inherit;False;SH_UETimeWithSpeedVariable;-1;;44;670908ee87f7f324381c8362e2e5e0fc;0;2;5;FLOAT3;0,0,0;False;4;FLOAT;0.25;False;2;FLOAT3;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;101;-4352,1392;Inherit;False;Property;_RiverTimeModulationSize;River Time Modulation Size;22;0;Create;True;0;0;0;False;0;False;0.64;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CrossProductOpNode;70;-4832,448;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ExpOpNode;66;-4512,640;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;99;-3984,1488;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;100;-4083.121,1393.471;Inherit;False;SH_UEWorldPosition-XY;-1;;45;111f0f055447f1742a757c7ecbbd9da7;0;1;4;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.BreakToComponentsNode;62;-4384,640;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.CommentaryNode;76;-3698,14;Inherit;False;1445.463;1030.872;Comment;13;59;58;54;57;52;53;55;60;50;51;56;48;46;Flowmap;1,1,1,1;0;0
Node;AmplifyShaderEditor.ComponentMaskNode;64;-4512,448;Inherit;False;True;True;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;98;-3808,1424;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;97;-3584,1424;Inherit;True;Property;_Noise;Noise;30;1;[HideInInspector];Create;True;0;0;0;False;0;False;-1;118d5f6d7e4988842a233ec166eb3816;118d5f6d7e4988842a233ec166eb3816;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;58;-3438.481,928.8723;Inherit;False;Property;_RiverFoamScale;River Foam Scale;20;0;Create;True;0;0;0;False;0;False;5.12;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;61;-4048,448;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.WorldPosInputsNode;59;-3648,800;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;54;-3552,528;Inherit;False;Property;_RiverFlowmapSpeed;River Flowmap Speed;19;0;Create;True;0;0;0;False;0;False;0.003;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;52;-3200,528;Inherit;False;Constant;_Vector1;Vector 1;17;0;Create;True;0;0;0;False;0;False;1,-1;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleDivideOpNode;57;-3248,800;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;53;-3328,448;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;96;-3184,1456;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.25;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;60;-3158.31,683.4174;Inherit;False;Property;_UseMipBias;Use Mip Bias;9;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;94;-2960,1552;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TexturePropertyNode;50;-3200,256;Inherit;True;Property;_Texture3;Texture 3;29;3;[HideInInspector];[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;2de9dc5047bf17b449a3a4a5d7757144;2de9dc5047bf17b449a3a4a5d7757144;True;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.ComponentMaskNode;56;-3120,800;Inherit;False;True;True;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;48;-3200,64;Inherit;True;Property;_Texture2;Texture 2;28;2;[HideInInspector];[NoScaleOffset];Create;True;0;0;0;False;0;False;cefde71f6e50f6b409631f543d98a4c3;cefde71f6e50f6b409631f543d98a4c3;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;51;-3040,448;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;55;-2962.42,899.6539;Inherit;False;Constant;_MipBias;Mip Bias;30;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;46;-2615.537,409.3686;Inherit;False;SH_UEFlowmaps;23;;46;2bacb256711f8e747822b1b1e7e19f16;0;9;46;SAMPLER2D;;False;47;SAMPLER2D;;False;14;FLOAT2;0.1,0;False;52;FLOAT2;0.5,0.5;False;11;FLOAT;0;False;90;INT;1;False;21;FLOAT2;0,0;False;25;FLOAT2;1024,1024;False;27;FLOAT;0;False;5;FLOAT3;72;FLOAT;73;COLOR;67;FLOAT4;63;FLOAT2;62
Node;AmplifyShaderEditor.ComponentMaskNode;34;-2272,400;Inherit;False;True;False;False;False;1;0;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;47;-2154.313,-626;Inherit;False;2041.457;870;Comment;24;33;27;26;31;32;30;29;25;24;28;21;23;20;22;19;17;18;15;14;13;12;10;11;8;Foam Diffuse;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleAddOpNode;33;-2104.313,60.24281;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;31;-2042.372,-176.8137;Inherit;False;Property;_FoamHigh;Foam High;4;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;26;-1904,-464;Inherit;False;Property;_DistanceDivider;Distance Divider;0;0;Create;True;0;0;0;False;0;False;0.001;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;27;-1984,-576;Inherit;False;Property;_DISTANCE_TO_NEAREST_SURFACE;DISTANCE_TO_NEAREST_SURFACE;27;0;Create;True;0;0;0;False;0;False;100;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;32;-1968,-48;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;30;-1991.372,-294.8137;Inherit;False;Property;_FoamLow;Foam Low;6;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;28;-1728,-128;Inherit;False;Property;_FoamExp;Foam Exp;3;0;Create;True;0;0;0;False;0;False;8;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;24;-1648,-432;Inherit;False;Property;_DistancePower;Distance Power;1;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;25;-1600,-528;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;29;-1744,-256;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;23;-1440,-528;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;92;-2032,2016;Inherit;False;Property;_WaterOpacityMaskOffset;Water Opacity Mask Offset;15;0;Create;True;0;0;0;False;0;False;-24;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;21;-1504,-192;Inherit;False;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;20;-1536,-80;Inherit;False;Property;_FoamIntensity;Foam Intensity;5;0;Create;True;0;0;0;False;0;False;16;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;93;-1953.135,1326;Inherit;False;857.135;471.1511;Comment;8;83;81;82;85;79;84;86;78;Scattering and Absorption;0.5330188,0.9572074,1,1;0;0
Node;AmplifyShaderEditor.VertexColorNode;91;-1941.863,1837.363;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SaturateNode;22;-1296,-528;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;81;-1903.135,1381.525;Inherit;False;Property;_Scattering;Scattering;14;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,0.5019608;1,1,1,0.5019608;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleSubtractOpNode;90;-1696,1936;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;19;-1328,-160;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;83;-1902.849,1585.151;Float;False;Property;_Absorption;Absorption;10;1;[HDR];Create;True;0;0;0;False;0;False;1,1,1,0.5019608;1,1,1,0.5019608;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;82;-1696,1376;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;85;-1696,1584;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;63;-4103.021,709.5895;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ComponentMaskNode;18;-1184,-160;Inherit;False;True;False;False;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;17;-1156.553,-528.7216;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;89;-1536,1936;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;44;-2560,1216;Inherit;False;Property;_RiverFlowmapDetectionVelocity;River Flowmap Detection Velocity;18;0;Create;True;0;0;0;False;0;False;0.1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;79;-1488,1392;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LengthOpNode;45;-3712,1136;Inherit;False;1;0;FLOAT2;0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;84;-1488,1552;Inherit;False;2;0;FLOAT3;1,1,1;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;15;-929.9056,-238.5529;Inherit;False;Property;_FoamContrast;Foam Contrast;2;0;Create;True;0;0;0;False;0;False;2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;88;-1360,1872;Inherit;False;3;0;FLOAT;-1;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;14;-959.6994,-454.9001;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;41;-1479.278,636.2416;Inherit;False;Property;_RiverNormalFlatness;River Normal Flatness;21;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;87;-1196.018,1867.399;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;43;-2234.829,1134.025;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;86;-1344,1600;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;13;-736,-320;Inherit;False;SF_UECheapContrast;-1;;54;93d366414e6f1cc45b1077ffab365271;0;2;2;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;78;-1248,1392;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;1000,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;36;-5.748736,406.8551;Inherit;False;Property;_WaterRoughness;Water Roughness;8;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;39;-1112.109,426.58;Inherit;False;Constant;_Vector0;Vector 0;12;0;Create;True;0;0;0;False;0;False;0,0,1;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SaturateNode;12;-496,-320;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;11;-624,32;Inherit;False;Property;_BaseColor;Base Color;11;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SaturateNode;42;-2058.829,1134.025;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;77;-892.9052,1375.697;Inherit;False;SH_UEWaterCoefficientMask;-1;;55;4f38f01305ec95b469f1b4e64d6bca1f;0;3;6;FLOAT;1;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;3;FLOAT;0;FLOAT3;1;FLOAT3;2
Node;AmplifyShaderEditor.FunctionNode;40;-1176.109,586.5801;Inherit;False;SF_UEFlattenNormal;-1;;56;a744336ecfc2d7743b2d22dfa4eb0cf3;0;2;2;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;10;-624,-144;Inherit;False;Property;_FoamColor;Foam Color;13;1;[HDR];Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;8;-294.8559,-173.9892;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.OneMinusNode;37;186.2513,406.8551;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;38;-865.894,579.7025;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMaxOpNode;16;-144.5944,532.0673;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;35;202.2513,326.8552;Inherit;False;Property;_Specular;Specular;7;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;362.251,278.8553;Float;False;True;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;VFXA/SH_River;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;18;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=Transparent=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;38;Workflow;0;637921389669776900;Surface;1;637921386302130912;  Refraction Model;0;0;  Blend;0;0;Two Sided;1;0;Fragment Normal Space,InvertActionOnDeselection;0;0;Transmission;0;0;  Transmission Shadow;0.5,False,-1;0;Translucency;0;0;  Translucency Strength;1,False,-1;0;  Normal Distortion;0.5,False,-1;0;  Scattering;2,False,-1;0;  Direct;0.9,False,-1;0;  Ambient;0.1,False,-1;0;  Shadow;0.5,False,-1;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;GPU Instancing;1;0;LOD CrossFade;1;0;Built-in Fog;1;0;_FinalColorxAlpha;0;0;Meta Pass;1;0;Override Baked GI;0;0;Extra Pre Pass;0;0;DOTS Instancing;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,-1;0;  Type;0;0;  Tess;16,False,-1;0;  Min;10,False,-1;0;  Max;25,False,-1;0;  Edge Length;16,False,-1;0;  Max Displacement;25,False,-1;0;Write Depth;0;0;  Early Z;0;0;Vertex Position,InvertActionOnDeselection;1;0;0;8;False;True;True;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthNormals;0;6;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=DepthNormals;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;GBuffer;0;7;GBuffer;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;5;False;-1;10;False;-1;1;1;False;-1;10;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;2;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalGBuffer;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;73;0;71;3
WireConnection;73;1;74;0
WireConnection;69;0;73;0
WireConnection;69;1;71;3
WireConnection;67;0;68;0
WireConnection;67;1;69;0
WireConnection;103;5;105;0
WireConnection;70;0;71;0
WireConnection;70;1;108;0
WireConnection;66;0;67;0
WireConnection;99;0;102;0
WireConnection;99;1;103;0
WireConnection;100;4;101;0
WireConnection;62;0;66;0
WireConnection;64;0;70;0
WireConnection;98;0;100;0
WireConnection;98;1;99;0
WireConnection;97;1;98;0
WireConnection;61;0;62;0
WireConnection;61;1;64;0
WireConnection;57;0;59;0
WireConnection;57;1;58;0
WireConnection;53;0;61;0
WireConnection;53;1;54;0
WireConnection;96;0;97;1
WireConnection;94;0;96;0
WireConnection;94;1;103;0
WireConnection;56;0;57;0
WireConnection;51;0;53;0
WireConnection;51;1;52;0
WireConnection;46;46;48;0
WireConnection;46;47;50;0
WireConnection;46;14;51;0
WireConnection;46;11;94;0
WireConnection;46;90;60;0
WireConnection;46;21;56;0
WireConnection;46;27;55;0
WireConnection;34;0;46;72
WireConnection;33;0;34;0
WireConnection;32;0;33;0
WireConnection;25;0;27;0
WireConnection;25;1;26;0
WireConnection;29;0;30;0
WireConnection;29;1;31;0
WireConnection;29;2;32;0
WireConnection;23;0;25;0
WireConnection;23;1;24;0
WireConnection;21;0;29;0
WireConnection;21;1;28;0
WireConnection;22;0;23;0
WireConnection;90;0;91;3
WireConnection;90;1;92;0
WireConnection;19;0;21;0
WireConnection;19;1;20;0
WireConnection;82;0;81;0
WireConnection;85;0;83;0
WireConnection;63;0;62;0
WireConnection;63;1;64;0
WireConnection;18;0;19;0
WireConnection;17;0;22;0
WireConnection;89;0;90;0
WireConnection;79;0;82;0
WireConnection;79;1;81;4
WireConnection;45;0;63;0
WireConnection;84;1;85;0
WireConnection;88;2;89;0
WireConnection;14;0;17;0
WireConnection;14;1;18;0
WireConnection;87;0;88;0
WireConnection;43;0;45;0
WireConnection;43;1;44;0
WireConnection;86;0;84;0
WireConnection;86;1;83;4
WireConnection;13;2;14;0
WireConnection;13;1;15;0
WireConnection;78;0;79;0
WireConnection;12;0;13;0
WireConnection;42;0;43;0
WireConnection;77;6;87;0
WireConnection;77;7;78;0
WireConnection;77;8;86;0
WireConnection;40;2;46;67
WireConnection;40;4;41;0
WireConnection;8;0;11;0
WireConnection;8;1;10;0
WireConnection;8;2;12;0
WireConnection;37;0;36;0
WireConnection;38;0;39;0
WireConnection;38;1;40;0
WireConnection;38;2;42;0
WireConnection;16;0;12;0
WireConnection;16;1;77;0
WireConnection;1;0;8;0
WireConnection;1;1;38;0
WireConnection;1;9;35;0
WireConnection;1;4;37;0
WireConnection;1;6;16;0
ASEEND*/
//CHKSM=62B7E296974D5C40E0989AD69912ED62686F09DC