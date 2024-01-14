// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "SH_Waterfall_Sprite"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_Hardness("Hardness", Float) = 0.1889
		_MipBias("Mip Bias", Float) = 1
		_NoiseContrast("Noise Contrast", Float) = 0.5
		_NoiseScale("Noise Scale", Float) = 1
		_NoiseSpeed("Noise Speed", Float) = 0
		_Radius("Radius", Float) = 0.5
		_RotateSpeed("Rotate Speed", Float) = 1
		[Toggle(_VERTEXCOLORSPHEREMASK_ON)] _VertexColorSphereMask("Vertex Color / Sphere Mask", Float) = 0
		[NoScaleOffset]_FlowDiffuse("Flow Diffuse", 2D) = "white" {}
		[NoScaleOffset]_FlowMap("Flow Map", 2D) = "white" {}
		[NoScaleOffset][Normal]_FlowNormal("Flow Normal", 2D) = "white" {}
		[HideInInspector]_TextureSample0("Texture Sample 0", 2D) = "white" {}
		_ParticleRandomValue("Particle Random Value", Float) = 1
		_ParticleRelativeTime("Particle Relative Time", Float) = 1
		[ASEEnd]_AlphaClipThreshold("Alpha Clip Threshold", Range( 0 , 1)) = 0.3333

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
			#define _ALPHATEST_ON 1
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

			#define ASE_NEEDS_FRAG_SCREEN_POSITION
			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _VERTEXCOLORSPHEREMASK_ON


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
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float _ParticleRandomValue;
			float _ParticleRelativeTime;
			float _NoiseSpeed;
			float _NoiseScale;
			float _RotateSpeed;
			float _MipBias;
			float _NoiseContrast;
			float _Radius;
			float _Hardness;
			float _AlphaClipThreshold;
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
			sampler2D _FlowDiffuse;
			sampler2D _TextureSample0;
			sampler2D _FlowMap;
			sampler2D _FlowNormal;


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord7.xy = v.texcoord.xy;
				o.ase_color = v.ase_color;
				
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

				int temp_output_90_0_g42 = 1;
				float2 texCoord27 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_21_0_g42 = texCoord27;
				float2 texCoord6_g42 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float3 temp_cast_1 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g49 = ( temp_cast_1 * 0.5 );
				float3 temp_output_75_0_g42 = temp_output_3_0_g49;
				float temp_output_11_0_g42 = ( ( tex2D( _TextureSample0, frac( ( float3( texCoord6_g42 ,  0.0 ) + ( temp_output_75_0_g42 * float3( 0.02,0.02,0.02 ) ) ) ).xy ).r * 0.25 ) + temp_output_75_0_g42 ).x;
				float2 appendResult42 = (float2(0.0 , _NoiseSpeed));
				float2 texCoord40 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float2 panner39 = ( ( _ParticleRelativeTime + _ParticleRandomValue ) * appendResult42 + texCoord40);
				float2 texCoord53 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float cos49 = cos( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float sin49 = sin( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float2 rotator49 = mul( texCoord53 - float2( 0.5,0.5 ) , float2x2( cos49 , -sin49 , sin49 , cos49 )) + float2( 0.5,0.5 );
				float2 temp_output_15_0_g42 = ( ( ( (( ( ( _ParticleRandomValue * 12.0 ) + panner39 ) * _NoiseScale )).xy + (tex2D( _FlowMap, rotator49 )).rg ) / float2( 2,0 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g42 = ( frac( ( temp_output_11_0_g42 - 0.5 ) ) * temp_output_15_0_g42 );
				float2 temp_output_19_0_g42 = ( temp_output_21_0_g42 + temp_output_16_0_g42 );
				float2 temp_output_51_0_g42 = ( frac( temp_output_19_0_g42 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g45 = temp_output_21_0_g42.x;
				float2 temp_output_6_0_g45 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g45 = ( ddx( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult9_g45 = dot( temp_output_8_0_g45 , temp_output_8_0_g45 );
				float2 temp_output_7_0_g45 = ( ddy( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult10_g45 = dot( temp_output_7_0_g45 , temp_output_7_0_g45 );
				float ifLocalVar11_g45 = 0;
				if( dotResult9_g45 > dotResult10_g45 )
				ifLocalVar11_g45 = dotResult9_g45;
				else if( dotResult9_g45 < dotResult10_g45 )
				ifLocalVar11_g45 = dotResult10_g45;
				float temp_output_26_0_g42 = ( log2( sqrt( ifLocalVar11_g45 ) ) + _MipBias );
				float4 tex2DNode49_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar88_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar88_g42 = tex2D( _FlowDiffuse, temp_output_51_0_g42 );
				else
				ifLocalVar88_g42 = tex2DNode49_g42;
				float temp_output_12_0_g42 = frac( temp_output_11_0_g42 );
				float2 temp_output_13_0_g42 = ( temp_output_15_0_g42 * temp_output_12_0_g42 );
				float2 temp_output_22_0_g42 = ( temp_output_13_0_g42 + temp_output_21_0_g42 );
				float4 tex2DNode32_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar83_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar83_g42 = tex2D( _FlowDiffuse, temp_output_22_0_g42 );
				else
				ifLocalVar83_g42 = tex2DNode32_g42;
				float temp_output_2_0_g46 = temp_output_12_0_g42;
				float temp_output_3_0_g46 = 1.0;
				float temp_output_4_0_g46 = ( temp_output_2_0_g46 + ( 0.25 * temp_output_3_0_g46 ) );
				float ifLocalVar24_g46 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g46 = temp_output_2_0_g46;
				else
				ifLocalVar24_g46 = temp_output_4_0_g46;
				float temp_output_7_0_g46 = frac( ( ifLocalVar24_g46 / temp_output_3_0_g46 ) );
				float temp_output_8_0_g46 = ( 2.0 * temp_output_7_0_g46 );
				float temp_output_12_0_g46 = floor( temp_output_8_0_g46 );
				float lerpResult13_g46 = lerp( temp_output_8_0_g46 , ( 2.0 * ( 1.0 - temp_output_7_0_g46 ) ) , temp_output_12_0_g46);
				float temp_output_78_0_g42 = lerpResult13_g46;
				float4 lerpResult69_g42 = lerp( ifLocalVar88_g42 , ifLocalVar83_g42 , temp_output_78_0_g42);
				float3 temp_output_57_72 = (lerpResult69_g42).rgb;
				
				float4 tex2DNode48_g42 = tex2Dlod( _FlowNormal, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar89_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar89_g42 = tex2D( _FlowNormal, temp_output_51_0_g42 );
				else
				ifLocalVar89_g42 = tex2DNode48_g42;
				float4 tex2DNode34_g42 = tex2Dlod( _FlowNormal, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar84_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar84_g42 = tex2D( _FlowNormal, temp_output_22_0_g42 );
				else
				ifLocalVar84_g42 = tex2DNode34_g42;
				float4 lerpResult68_g42 = lerp( ifLocalVar89_g42 , ifLocalVar84_g42 , temp_output_78_0_g42);
				
				float4 ase_screenPosNorm = ScreenPos / ScreenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen56 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither56 = Dither4x4Bayer( fmod(clipScreen56.x, 4), fmod(clipScreen56.y, 4) );
				float temp_output_1_0_g50 = _NoiseContrast;
				float2 texCoord23 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float lerpResult17_g4 = lerp( 1.0 , 0.01 , _Hardness);
				#ifdef _VERTEXCOLORSPHEREMASK_ON
				float staticSwitch15 = IN.ase_color.r;
				#else
				float staticSwitch15 = pow( ( 1.0 - saturate( ( distance( texCoord23 , float2( 0.5,0.5 ) ) / _Radius ) ) ) , lerpResult17_g4 );
				#endif
				float lerpResult5_g50 = lerp( ( 0.0 - temp_output_1_0_g50 ) , ( temp_output_1_0_g50 + 1.0 ) , ( temp_output_57_72 * staticSwitch15 ).x);
				dither56 = step( dither56, ( IN.ase_color.a * saturate( lerpResult5_g50 ) ) );
				
				float3 Albedo = ( float4( temp_output_57_72 , 0.0 ) * IN.ase_color ).rgb;
				float3 Normal = lerpResult68_g42.rgb;
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = 0;
				float Smoothness = 0.5;
				float Occlusion = 1;
				float Alpha = dither56;
				float AlphaClipThreshold = _AlphaClipThreshold;
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
			#define _ALPHATEST_ON 1
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

			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _VERTEXCOLORSPHEREMASK_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
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
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float _ParticleRandomValue;
			float _ParticleRelativeTime;
			float _NoiseSpeed;
			float _NoiseScale;
			float _RotateSpeed;
			float _MipBias;
			float _NoiseContrast;
			float _Radius;
			float _Hardness;
			float _AlphaClipThreshold;
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
			sampler2D _FlowDiffuse;
			sampler2D _TextureSample0;
			sampler2D _FlowMap;


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

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

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord2 = screenPos;
				
				o.ase_color = v.ase_color;
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.zw = 0;
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
				float4 ase_texcoord : TEXCOORD0;

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
				o.ase_texcoord = v.ase_texcoord;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
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

				float4 screenPos = IN.ase_texcoord2;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen56 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither56 = Dither4x4Bayer( fmod(clipScreen56.x, 4), fmod(clipScreen56.y, 4) );
				float temp_output_1_0_g50 = _NoiseContrast;
				int temp_output_90_0_g42 = 1;
				float2 texCoord27 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_21_0_g42 = texCoord27;
				float2 texCoord6_g42 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float3 temp_cast_1 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g49 = ( temp_cast_1 * 0.5 );
				float3 temp_output_75_0_g42 = temp_output_3_0_g49;
				float temp_output_11_0_g42 = ( ( tex2D( _TextureSample0, frac( ( float3( texCoord6_g42 ,  0.0 ) + ( temp_output_75_0_g42 * float3( 0.02,0.02,0.02 ) ) ) ).xy ).r * 0.25 ) + temp_output_75_0_g42 ).x;
				float2 appendResult42 = (float2(0.0 , _NoiseSpeed));
				float2 texCoord40 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 panner39 = ( ( _ParticleRelativeTime + _ParticleRandomValue ) * appendResult42 + texCoord40);
				float2 texCoord53 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float cos49 = cos( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float sin49 = sin( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float2 rotator49 = mul( texCoord53 - float2( 0.5,0.5 ) , float2x2( cos49 , -sin49 , sin49 , cos49 )) + float2( 0.5,0.5 );
				float2 temp_output_15_0_g42 = ( ( ( (( ( ( _ParticleRandomValue * 12.0 ) + panner39 ) * _NoiseScale )).xy + (tex2D( _FlowMap, rotator49 )).rg ) / float2( 2,0 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g42 = ( frac( ( temp_output_11_0_g42 - 0.5 ) ) * temp_output_15_0_g42 );
				float2 temp_output_19_0_g42 = ( temp_output_21_0_g42 + temp_output_16_0_g42 );
				float2 temp_output_51_0_g42 = ( frac( temp_output_19_0_g42 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g45 = temp_output_21_0_g42.x;
				float2 temp_output_6_0_g45 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g45 = ( ddx( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult9_g45 = dot( temp_output_8_0_g45 , temp_output_8_0_g45 );
				float2 temp_output_7_0_g45 = ( ddy( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult10_g45 = dot( temp_output_7_0_g45 , temp_output_7_0_g45 );
				float ifLocalVar11_g45 = 0;
				if( dotResult9_g45 > dotResult10_g45 )
				ifLocalVar11_g45 = dotResult9_g45;
				else if( dotResult9_g45 < dotResult10_g45 )
				ifLocalVar11_g45 = dotResult10_g45;
				float temp_output_26_0_g42 = ( log2( sqrt( ifLocalVar11_g45 ) ) + _MipBias );
				float4 tex2DNode49_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar88_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar88_g42 = tex2D( _FlowDiffuse, temp_output_51_0_g42 );
				else
				ifLocalVar88_g42 = tex2DNode49_g42;
				float temp_output_12_0_g42 = frac( temp_output_11_0_g42 );
				float2 temp_output_13_0_g42 = ( temp_output_15_0_g42 * temp_output_12_0_g42 );
				float2 temp_output_22_0_g42 = ( temp_output_13_0_g42 + temp_output_21_0_g42 );
				float4 tex2DNode32_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar83_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar83_g42 = tex2D( _FlowDiffuse, temp_output_22_0_g42 );
				else
				ifLocalVar83_g42 = tex2DNode32_g42;
				float temp_output_2_0_g46 = temp_output_12_0_g42;
				float temp_output_3_0_g46 = 1.0;
				float temp_output_4_0_g46 = ( temp_output_2_0_g46 + ( 0.25 * temp_output_3_0_g46 ) );
				float ifLocalVar24_g46 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g46 = temp_output_2_0_g46;
				else
				ifLocalVar24_g46 = temp_output_4_0_g46;
				float temp_output_7_0_g46 = frac( ( ifLocalVar24_g46 / temp_output_3_0_g46 ) );
				float temp_output_8_0_g46 = ( 2.0 * temp_output_7_0_g46 );
				float temp_output_12_0_g46 = floor( temp_output_8_0_g46 );
				float lerpResult13_g46 = lerp( temp_output_8_0_g46 , ( 2.0 * ( 1.0 - temp_output_7_0_g46 ) ) , temp_output_12_0_g46);
				float temp_output_78_0_g42 = lerpResult13_g46;
				float4 lerpResult69_g42 = lerp( ifLocalVar88_g42 , ifLocalVar83_g42 , temp_output_78_0_g42);
				float3 temp_output_57_72 = (lerpResult69_g42).rgb;
				float2 texCoord23 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float lerpResult17_g4 = lerp( 1.0 , 0.01 , _Hardness);
				#ifdef _VERTEXCOLORSPHEREMASK_ON
				float staticSwitch15 = IN.ase_color.r;
				#else
				float staticSwitch15 = pow( ( 1.0 - saturate( ( distance( texCoord23 , float2( 0.5,0.5 ) ) / _Radius ) ) ) , lerpResult17_g4 );
				#endif
				float lerpResult5_g50 = lerp( ( 0.0 - temp_output_1_0_g50 ) , ( temp_output_1_0_g50 + 1.0 ) , ( temp_output_57_72 * staticSwitch15 ).x);
				dither56 = step( dither56, ( IN.ase_color.a * saturate( lerpResult5_g50 ) ) );
				
				float Alpha = dither56;
				float AlphaClipThreshold = _AlphaClipThreshold;
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
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _VERTEXCOLORSPHEREMASK_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
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
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float _ParticleRandomValue;
			float _ParticleRelativeTime;
			float _NoiseSpeed;
			float _NoiseScale;
			float _RotateSpeed;
			float _MipBias;
			float _NoiseContrast;
			float _Radius;
			float _Hardness;
			float _AlphaClipThreshold;
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
			sampler2D _FlowDiffuse;
			sampler2D _TextureSample0;
			sampler2D _FlowMap;


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord2 = screenPos;
				
				o.ase_color = v.ase_color;
				o.ase_texcoord3.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.zw = 0;
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
				float4 ase_texcoord : TEXCOORD0;

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
				o.ase_texcoord = v.ase_texcoord;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
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

				float4 screenPos = IN.ase_texcoord2;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen56 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither56 = Dither4x4Bayer( fmod(clipScreen56.x, 4), fmod(clipScreen56.y, 4) );
				float temp_output_1_0_g50 = _NoiseContrast;
				int temp_output_90_0_g42 = 1;
				float2 texCoord27 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_21_0_g42 = texCoord27;
				float2 texCoord6_g42 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float3 temp_cast_1 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g49 = ( temp_cast_1 * 0.5 );
				float3 temp_output_75_0_g42 = temp_output_3_0_g49;
				float temp_output_11_0_g42 = ( ( tex2D( _TextureSample0, frac( ( float3( texCoord6_g42 ,  0.0 ) + ( temp_output_75_0_g42 * float3( 0.02,0.02,0.02 ) ) ) ).xy ).r * 0.25 ) + temp_output_75_0_g42 ).x;
				float2 appendResult42 = (float2(0.0 , _NoiseSpeed));
				float2 texCoord40 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float2 panner39 = ( ( _ParticleRelativeTime + _ParticleRandomValue ) * appendResult42 + texCoord40);
				float2 texCoord53 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float cos49 = cos( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float sin49 = sin( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float2 rotator49 = mul( texCoord53 - float2( 0.5,0.5 ) , float2x2( cos49 , -sin49 , sin49 , cos49 )) + float2( 0.5,0.5 );
				float2 temp_output_15_0_g42 = ( ( ( (( ( ( _ParticleRandomValue * 12.0 ) + panner39 ) * _NoiseScale )).xy + (tex2D( _FlowMap, rotator49 )).rg ) / float2( 2,0 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g42 = ( frac( ( temp_output_11_0_g42 - 0.5 ) ) * temp_output_15_0_g42 );
				float2 temp_output_19_0_g42 = ( temp_output_21_0_g42 + temp_output_16_0_g42 );
				float2 temp_output_51_0_g42 = ( frac( temp_output_19_0_g42 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g45 = temp_output_21_0_g42.x;
				float2 temp_output_6_0_g45 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g45 = ( ddx( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult9_g45 = dot( temp_output_8_0_g45 , temp_output_8_0_g45 );
				float2 temp_output_7_0_g45 = ( ddy( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult10_g45 = dot( temp_output_7_0_g45 , temp_output_7_0_g45 );
				float ifLocalVar11_g45 = 0;
				if( dotResult9_g45 > dotResult10_g45 )
				ifLocalVar11_g45 = dotResult9_g45;
				else if( dotResult9_g45 < dotResult10_g45 )
				ifLocalVar11_g45 = dotResult10_g45;
				float temp_output_26_0_g42 = ( log2( sqrt( ifLocalVar11_g45 ) ) + _MipBias );
				float4 tex2DNode49_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar88_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar88_g42 = tex2D( _FlowDiffuse, temp_output_51_0_g42 );
				else
				ifLocalVar88_g42 = tex2DNode49_g42;
				float temp_output_12_0_g42 = frac( temp_output_11_0_g42 );
				float2 temp_output_13_0_g42 = ( temp_output_15_0_g42 * temp_output_12_0_g42 );
				float2 temp_output_22_0_g42 = ( temp_output_13_0_g42 + temp_output_21_0_g42 );
				float4 tex2DNode32_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar83_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar83_g42 = tex2D( _FlowDiffuse, temp_output_22_0_g42 );
				else
				ifLocalVar83_g42 = tex2DNode32_g42;
				float temp_output_2_0_g46 = temp_output_12_0_g42;
				float temp_output_3_0_g46 = 1.0;
				float temp_output_4_0_g46 = ( temp_output_2_0_g46 + ( 0.25 * temp_output_3_0_g46 ) );
				float ifLocalVar24_g46 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g46 = temp_output_2_0_g46;
				else
				ifLocalVar24_g46 = temp_output_4_0_g46;
				float temp_output_7_0_g46 = frac( ( ifLocalVar24_g46 / temp_output_3_0_g46 ) );
				float temp_output_8_0_g46 = ( 2.0 * temp_output_7_0_g46 );
				float temp_output_12_0_g46 = floor( temp_output_8_0_g46 );
				float lerpResult13_g46 = lerp( temp_output_8_0_g46 , ( 2.0 * ( 1.0 - temp_output_7_0_g46 ) ) , temp_output_12_0_g46);
				float temp_output_78_0_g42 = lerpResult13_g46;
				float4 lerpResult69_g42 = lerp( ifLocalVar88_g42 , ifLocalVar83_g42 , temp_output_78_0_g42);
				float3 temp_output_57_72 = (lerpResult69_g42).rgb;
				float2 texCoord23 = IN.ase_texcoord3.xy * float2( 1,1 ) + float2( 0,0 );
				float lerpResult17_g4 = lerp( 1.0 , 0.01 , _Hardness);
				#ifdef _VERTEXCOLORSPHEREMASK_ON
				float staticSwitch15 = IN.ase_color.r;
				#else
				float staticSwitch15 = pow( ( 1.0 - saturate( ( distance( texCoord23 , float2( 0.5,0.5 ) ) / _Radius ) ) ) , lerpResult17_g4 );
				#endif
				float lerpResult5_g50 = lerp( ( 0.0 - temp_output_1_0_g50 ) , ( temp_output_1_0_g50 + 1.0 ) , ( temp_output_57_72 * staticSwitch15 ).x);
				dither56 = step( dither56, ( IN.ase_color.a * saturate( lerpResult5_g50 ) ) );
				
				float Alpha = dither56;
				float AlphaClipThreshold = _AlphaClipThreshold;
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
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _VERTEXCOLORSPHEREMASK_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_texcoord : TEXCOORD0;
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
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float _ParticleRandomValue;
			float _ParticleRelativeTime;
			float _NoiseSpeed;
			float _NoiseScale;
			float _RotateSpeed;
			float _MipBias;
			float _NoiseContrast;
			float _Radius;
			float _Hardness;
			float _AlphaClipThreshold;
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
			sampler2D _FlowDiffuse;
			sampler2D _TextureSample0;
			sampler2D _FlowMap;


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord3 = screenPos;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
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
				float4 ase_texcoord : TEXCOORD0;
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
				o.ase_texcoord = v.ase_texcoord;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
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

				int temp_output_90_0_g42 = 1;
				float2 texCoord27 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_21_0_g42 = texCoord27;
				float2 texCoord6_g42 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float3 temp_cast_1 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g49 = ( temp_cast_1 * 0.5 );
				float3 temp_output_75_0_g42 = temp_output_3_0_g49;
				float temp_output_11_0_g42 = ( ( tex2D( _TextureSample0, frac( ( float3( texCoord6_g42 ,  0.0 ) + ( temp_output_75_0_g42 * float3( 0.02,0.02,0.02 ) ) ) ).xy ).r * 0.25 ) + temp_output_75_0_g42 ).x;
				float2 appendResult42 = (float2(0.0 , _NoiseSpeed));
				float2 texCoord40 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 panner39 = ( ( _ParticleRelativeTime + _ParticleRandomValue ) * appendResult42 + texCoord40);
				float2 texCoord53 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float cos49 = cos( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float sin49 = sin( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float2 rotator49 = mul( texCoord53 - float2( 0.5,0.5 ) , float2x2( cos49 , -sin49 , sin49 , cos49 )) + float2( 0.5,0.5 );
				float2 temp_output_15_0_g42 = ( ( ( (( ( ( _ParticleRandomValue * 12.0 ) + panner39 ) * _NoiseScale )).xy + (tex2D( _FlowMap, rotator49 )).rg ) / float2( 2,0 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g42 = ( frac( ( temp_output_11_0_g42 - 0.5 ) ) * temp_output_15_0_g42 );
				float2 temp_output_19_0_g42 = ( temp_output_21_0_g42 + temp_output_16_0_g42 );
				float2 temp_output_51_0_g42 = ( frac( temp_output_19_0_g42 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g45 = temp_output_21_0_g42.x;
				float2 temp_output_6_0_g45 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g45 = ( ddx( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult9_g45 = dot( temp_output_8_0_g45 , temp_output_8_0_g45 );
				float2 temp_output_7_0_g45 = ( ddy( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult10_g45 = dot( temp_output_7_0_g45 , temp_output_7_0_g45 );
				float ifLocalVar11_g45 = 0;
				if( dotResult9_g45 > dotResult10_g45 )
				ifLocalVar11_g45 = dotResult9_g45;
				else if( dotResult9_g45 < dotResult10_g45 )
				ifLocalVar11_g45 = dotResult10_g45;
				float temp_output_26_0_g42 = ( log2( sqrt( ifLocalVar11_g45 ) ) + _MipBias );
				float4 tex2DNode49_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar88_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar88_g42 = tex2D( _FlowDiffuse, temp_output_51_0_g42 );
				else
				ifLocalVar88_g42 = tex2DNode49_g42;
				float temp_output_12_0_g42 = frac( temp_output_11_0_g42 );
				float2 temp_output_13_0_g42 = ( temp_output_15_0_g42 * temp_output_12_0_g42 );
				float2 temp_output_22_0_g42 = ( temp_output_13_0_g42 + temp_output_21_0_g42 );
				float4 tex2DNode32_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar83_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar83_g42 = tex2D( _FlowDiffuse, temp_output_22_0_g42 );
				else
				ifLocalVar83_g42 = tex2DNode32_g42;
				float temp_output_2_0_g46 = temp_output_12_0_g42;
				float temp_output_3_0_g46 = 1.0;
				float temp_output_4_0_g46 = ( temp_output_2_0_g46 + ( 0.25 * temp_output_3_0_g46 ) );
				float ifLocalVar24_g46 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g46 = temp_output_2_0_g46;
				else
				ifLocalVar24_g46 = temp_output_4_0_g46;
				float temp_output_7_0_g46 = frac( ( ifLocalVar24_g46 / temp_output_3_0_g46 ) );
				float temp_output_8_0_g46 = ( 2.0 * temp_output_7_0_g46 );
				float temp_output_12_0_g46 = floor( temp_output_8_0_g46 );
				float lerpResult13_g46 = lerp( temp_output_8_0_g46 , ( 2.0 * ( 1.0 - temp_output_7_0_g46 ) ) , temp_output_12_0_g46);
				float temp_output_78_0_g42 = lerpResult13_g46;
				float4 lerpResult69_g42 = lerp( ifLocalVar88_g42 , ifLocalVar83_g42 , temp_output_78_0_g42);
				float3 temp_output_57_72 = (lerpResult69_g42).rgb;
				
				float4 screenPos = IN.ase_texcoord3;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen56 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither56 = Dither4x4Bayer( fmod(clipScreen56.x, 4), fmod(clipScreen56.y, 4) );
				float temp_output_1_0_g50 = _NoiseContrast;
				float2 texCoord23 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float lerpResult17_g4 = lerp( 1.0 , 0.01 , _Hardness);
				#ifdef _VERTEXCOLORSPHEREMASK_ON
				float staticSwitch15 = IN.ase_color.r;
				#else
				float staticSwitch15 = pow( ( 1.0 - saturate( ( distance( texCoord23 , float2( 0.5,0.5 ) ) / _Radius ) ) ) , lerpResult17_g4 );
				#endif
				float lerpResult5_g50 = lerp( ( 0.0 - temp_output_1_0_g50 ) , ( temp_output_1_0_g50 + 1.0 ) , ( temp_output_57_72 * staticSwitch15 ).x);
				dither56 = step( dither56, ( IN.ase_color.a * saturate( lerpResult5_g50 ) ) );
				
				
				float3 Albedo = ( float4( temp_output_57_72 , 0.0 ) * IN.ase_color ).rgb;
				float3 Emission = 0;
				float Alpha = dither56;
				float AlphaClipThreshold = _AlphaClipThreshold;

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
			#define _ALPHATEST_ON 1
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
			
			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _VERTEXCOLORSPHEREMASK_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
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
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float _ParticleRandomValue;
			float _ParticleRelativeTime;
			float _NoiseSpeed;
			float _NoiseScale;
			float _RotateSpeed;
			float _MipBias;
			float _NoiseContrast;
			float _Radius;
			float _Hardness;
			float _AlphaClipThreshold;
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
			sampler2D _FlowDiffuse;
			sampler2D _TextureSample0;
			sampler2D _FlowMap;


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord3 = screenPos;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
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
				float4 ase_texcoord : TEXCOORD0;
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
				o.ase_texcoord = v.ase_texcoord;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
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

				int temp_output_90_0_g42 = 1;
				float2 texCoord27 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_21_0_g42 = texCoord27;
				float2 texCoord6_g42 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float3 temp_cast_1 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g49 = ( temp_cast_1 * 0.5 );
				float3 temp_output_75_0_g42 = temp_output_3_0_g49;
				float temp_output_11_0_g42 = ( ( tex2D( _TextureSample0, frac( ( float3( texCoord6_g42 ,  0.0 ) + ( temp_output_75_0_g42 * float3( 0.02,0.02,0.02 ) ) ) ).xy ).r * 0.25 ) + temp_output_75_0_g42 ).x;
				float2 appendResult42 = (float2(0.0 , _NoiseSpeed));
				float2 texCoord40 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 panner39 = ( ( _ParticleRelativeTime + _ParticleRandomValue ) * appendResult42 + texCoord40);
				float2 texCoord53 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float cos49 = cos( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float sin49 = sin( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float2 rotator49 = mul( texCoord53 - float2( 0.5,0.5 ) , float2x2( cos49 , -sin49 , sin49 , cos49 )) + float2( 0.5,0.5 );
				float2 temp_output_15_0_g42 = ( ( ( (( ( ( _ParticleRandomValue * 12.0 ) + panner39 ) * _NoiseScale )).xy + (tex2D( _FlowMap, rotator49 )).rg ) / float2( 2,0 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g42 = ( frac( ( temp_output_11_0_g42 - 0.5 ) ) * temp_output_15_0_g42 );
				float2 temp_output_19_0_g42 = ( temp_output_21_0_g42 + temp_output_16_0_g42 );
				float2 temp_output_51_0_g42 = ( frac( temp_output_19_0_g42 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g45 = temp_output_21_0_g42.x;
				float2 temp_output_6_0_g45 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g45 = ( ddx( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult9_g45 = dot( temp_output_8_0_g45 , temp_output_8_0_g45 );
				float2 temp_output_7_0_g45 = ( ddy( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult10_g45 = dot( temp_output_7_0_g45 , temp_output_7_0_g45 );
				float ifLocalVar11_g45 = 0;
				if( dotResult9_g45 > dotResult10_g45 )
				ifLocalVar11_g45 = dotResult9_g45;
				else if( dotResult9_g45 < dotResult10_g45 )
				ifLocalVar11_g45 = dotResult10_g45;
				float temp_output_26_0_g42 = ( log2( sqrt( ifLocalVar11_g45 ) ) + _MipBias );
				float4 tex2DNode49_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar88_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar88_g42 = tex2D( _FlowDiffuse, temp_output_51_0_g42 );
				else
				ifLocalVar88_g42 = tex2DNode49_g42;
				float temp_output_12_0_g42 = frac( temp_output_11_0_g42 );
				float2 temp_output_13_0_g42 = ( temp_output_15_0_g42 * temp_output_12_0_g42 );
				float2 temp_output_22_0_g42 = ( temp_output_13_0_g42 + temp_output_21_0_g42 );
				float4 tex2DNode32_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar83_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar83_g42 = tex2D( _FlowDiffuse, temp_output_22_0_g42 );
				else
				ifLocalVar83_g42 = tex2DNode32_g42;
				float temp_output_2_0_g46 = temp_output_12_0_g42;
				float temp_output_3_0_g46 = 1.0;
				float temp_output_4_0_g46 = ( temp_output_2_0_g46 + ( 0.25 * temp_output_3_0_g46 ) );
				float ifLocalVar24_g46 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g46 = temp_output_2_0_g46;
				else
				ifLocalVar24_g46 = temp_output_4_0_g46;
				float temp_output_7_0_g46 = frac( ( ifLocalVar24_g46 / temp_output_3_0_g46 ) );
				float temp_output_8_0_g46 = ( 2.0 * temp_output_7_0_g46 );
				float temp_output_12_0_g46 = floor( temp_output_8_0_g46 );
				float lerpResult13_g46 = lerp( temp_output_8_0_g46 , ( 2.0 * ( 1.0 - temp_output_7_0_g46 ) ) , temp_output_12_0_g46);
				float temp_output_78_0_g42 = lerpResult13_g46;
				float4 lerpResult69_g42 = lerp( ifLocalVar88_g42 , ifLocalVar83_g42 , temp_output_78_0_g42);
				float3 temp_output_57_72 = (lerpResult69_g42).rgb;
				
				float4 screenPos = IN.ase_texcoord3;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen56 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither56 = Dither4x4Bayer( fmod(clipScreen56.x, 4), fmod(clipScreen56.y, 4) );
				float temp_output_1_0_g50 = _NoiseContrast;
				float2 texCoord23 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float lerpResult17_g4 = lerp( 1.0 , 0.01 , _Hardness);
				#ifdef _VERTEXCOLORSPHEREMASK_ON
				float staticSwitch15 = IN.ase_color.r;
				#else
				float staticSwitch15 = pow( ( 1.0 - saturate( ( distance( texCoord23 , float2( 0.5,0.5 ) ) / _Radius ) ) ) , lerpResult17_g4 );
				#endif
				float lerpResult5_g50 = lerp( ( 0.0 - temp_output_1_0_g50 ) , ( temp_output_1_0_g50 + 1.0 ) , ( temp_output_57_72 * staticSwitch15 ).x);
				dither56 = step( dither56, ( IN.ase_color.a * saturate( lerpResult5_g50 ) ) );
				
				
				float3 Albedo = ( float4( temp_output_57_72 , 0.0 ) * IN.ase_color ).rgb;
				float Alpha = dither56;
				float AlphaClipThreshold = _AlphaClipThreshold;

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
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _VERTEXCOLORSPHEREMASK_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
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
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_color : COLOR;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float _ParticleRandomValue;
			float _ParticleRelativeTime;
			float _NoiseSpeed;
			float _NoiseScale;
			float _RotateSpeed;
			float _MipBias;
			float _NoiseContrast;
			float _Radius;
			float _Hardness;
			float _AlphaClipThreshold;
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
			sampler2D _FlowDiffuse;
			sampler2D _TextureSample0;
			sampler2D _FlowMap;


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord3 = screenPos;
				
				o.ase_color = v.ase_color;
				o.ase_texcoord4.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord4.zw = 0;
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
				float4 ase_texcoord : TEXCOORD0;

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
				o.ase_texcoord = v.ase_texcoord;
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
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
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

				float4 screenPos = IN.ase_texcoord3;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen56 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither56 = Dither4x4Bayer( fmod(clipScreen56.x, 4), fmod(clipScreen56.y, 4) );
				float temp_output_1_0_g50 = _NoiseContrast;
				int temp_output_90_0_g42 = 1;
				float2 texCoord27 = IN.ase_texcoord4.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_21_0_g42 = texCoord27;
				float2 texCoord6_g42 = IN.ase_texcoord4.xy * float2( 1,1 ) + float2( 0,0 );
				float3 temp_cast_1 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g49 = ( temp_cast_1 * 0.5 );
				float3 temp_output_75_0_g42 = temp_output_3_0_g49;
				float temp_output_11_0_g42 = ( ( tex2D( _TextureSample0, frac( ( float3( texCoord6_g42 ,  0.0 ) + ( temp_output_75_0_g42 * float3( 0.02,0.02,0.02 ) ) ) ).xy ).r * 0.25 ) + temp_output_75_0_g42 ).x;
				float2 appendResult42 = (float2(0.0 , _NoiseSpeed));
				float2 texCoord40 = IN.ase_texcoord4.xy * float2( 1,1 ) + float2( 0,0 );
				float2 panner39 = ( ( _ParticleRelativeTime + _ParticleRandomValue ) * appendResult42 + texCoord40);
				float2 texCoord53 = IN.ase_texcoord4.xy * float2( 1,1 ) + float2( 0,0 );
				float cos49 = cos( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float sin49 = sin( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float2 rotator49 = mul( texCoord53 - float2( 0.5,0.5 ) , float2x2( cos49 , -sin49 , sin49 , cos49 )) + float2( 0.5,0.5 );
				float2 temp_output_15_0_g42 = ( ( ( (( ( ( _ParticleRandomValue * 12.0 ) + panner39 ) * _NoiseScale )).xy + (tex2D( _FlowMap, rotator49 )).rg ) / float2( 2,0 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g42 = ( frac( ( temp_output_11_0_g42 - 0.5 ) ) * temp_output_15_0_g42 );
				float2 temp_output_19_0_g42 = ( temp_output_21_0_g42 + temp_output_16_0_g42 );
				float2 temp_output_51_0_g42 = ( frac( temp_output_19_0_g42 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g45 = temp_output_21_0_g42.x;
				float2 temp_output_6_0_g45 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g45 = ( ddx( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult9_g45 = dot( temp_output_8_0_g45 , temp_output_8_0_g45 );
				float2 temp_output_7_0_g45 = ( ddy( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult10_g45 = dot( temp_output_7_0_g45 , temp_output_7_0_g45 );
				float ifLocalVar11_g45 = 0;
				if( dotResult9_g45 > dotResult10_g45 )
				ifLocalVar11_g45 = dotResult9_g45;
				else if( dotResult9_g45 < dotResult10_g45 )
				ifLocalVar11_g45 = dotResult10_g45;
				float temp_output_26_0_g42 = ( log2( sqrt( ifLocalVar11_g45 ) ) + _MipBias );
				float4 tex2DNode49_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar88_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar88_g42 = tex2D( _FlowDiffuse, temp_output_51_0_g42 );
				else
				ifLocalVar88_g42 = tex2DNode49_g42;
				float temp_output_12_0_g42 = frac( temp_output_11_0_g42 );
				float2 temp_output_13_0_g42 = ( temp_output_15_0_g42 * temp_output_12_0_g42 );
				float2 temp_output_22_0_g42 = ( temp_output_13_0_g42 + temp_output_21_0_g42 );
				float4 tex2DNode32_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar83_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar83_g42 = tex2D( _FlowDiffuse, temp_output_22_0_g42 );
				else
				ifLocalVar83_g42 = tex2DNode32_g42;
				float temp_output_2_0_g46 = temp_output_12_0_g42;
				float temp_output_3_0_g46 = 1.0;
				float temp_output_4_0_g46 = ( temp_output_2_0_g46 + ( 0.25 * temp_output_3_0_g46 ) );
				float ifLocalVar24_g46 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g46 = temp_output_2_0_g46;
				else
				ifLocalVar24_g46 = temp_output_4_0_g46;
				float temp_output_7_0_g46 = frac( ( ifLocalVar24_g46 / temp_output_3_0_g46 ) );
				float temp_output_8_0_g46 = ( 2.0 * temp_output_7_0_g46 );
				float temp_output_12_0_g46 = floor( temp_output_8_0_g46 );
				float lerpResult13_g46 = lerp( temp_output_8_0_g46 , ( 2.0 * ( 1.0 - temp_output_7_0_g46 ) ) , temp_output_12_0_g46);
				float temp_output_78_0_g42 = lerpResult13_g46;
				float4 lerpResult69_g42 = lerp( ifLocalVar88_g42 , ifLocalVar83_g42 , temp_output_78_0_g42);
				float3 temp_output_57_72 = (lerpResult69_g42).rgb;
				float2 texCoord23 = IN.ase_texcoord4.xy * float2( 1,1 ) + float2( 0,0 );
				float lerpResult17_g4 = lerp( 1.0 , 0.01 , _Hardness);
				#ifdef _VERTEXCOLORSPHEREMASK_ON
				float staticSwitch15 = IN.ase_color.r;
				#else
				float staticSwitch15 = pow( ( 1.0 - saturate( ( distance( texCoord23 , float2( 0.5,0.5 ) ) / _Radius ) ) ) , lerpResult17_g4 );
				#endif
				float lerpResult5_g50 = lerp( ( 0.0 - temp_output_1_0_g50 ) , ( temp_output_1_0_g50 + 1.0 ) , ( temp_output_57_72 * staticSwitch15 ).x);
				dither56 = step( dither56, ( IN.ase_color.a * saturate( lerpResult5_g50 ) ) );
				
				float Alpha = dither56;
				float AlphaClipThreshold = _AlphaClipThreshold;
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
			#define _ALPHATEST_ON 1
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

			#define ASE_NEEDS_FRAG_SCREEN_POSITION
			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _VERTEXCOLORSPHEREMASK_ON


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
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float _ParticleRandomValue;
			float _ParticleRelativeTime;
			float _NoiseSpeed;
			float _NoiseScale;
			float _RotateSpeed;
			float _MipBias;
			float _NoiseContrast;
			float _Radius;
			float _Hardness;
			float _AlphaClipThreshold;
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
			sampler2D _FlowDiffuse;
			sampler2D _TextureSample0;
			sampler2D _FlowMap;
			sampler2D _FlowNormal;


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord7.xy = v.texcoord.xy;
				o.ase_color = v.ase_color;
				
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

				int temp_output_90_0_g42 = 1;
				float2 texCoord27 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float2 temp_output_21_0_g42 = texCoord27;
				float2 texCoord6_g42 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float3 temp_cast_1 = (_TimeParameters.x).xxx;
				float3 temp_output_3_0_g49 = ( temp_cast_1 * 0.5 );
				float3 temp_output_75_0_g42 = temp_output_3_0_g49;
				float temp_output_11_0_g42 = ( ( tex2D( _TextureSample0, frac( ( float3( texCoord6_g42 ,  0.0 ) + ( temp_output_75_0_g42 * float3( 0.02,0.02,0.02 ) ) ) ).xy ).r * 0.25 ) + temp_output_75_0_g42 ).x;
				float2 appendResult42 = (float2(0.0 , _NoiseSpeed));
				float2 texCoord40 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float2 panner39 = ( ( _ParticleRelativeTime + _ParticleRandomValue ) * appendResult42 + texCoord40);
				float2 texCoord53 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float cos49 = cos( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float sin49 = sin( ( ( _TimeParameters.x * _RotateSpeed ) * 0.25 ) );
				float2 rotator49 = mul( texCoord53 - float2( 0.5,0.5 ) , float2x2( cos49 , -sin49 , sin49 , cos49 )) + float2( 0.5,0.5 );
				float2 temp_output_15_0_g42 = ( ( ( (( ( ( _ParticleRandomValue * 12.0 ) + panner39 ) * _NoiseScale )).xy + (tex2D( _FlowMap, rotator49 )).rg ) / float2( 2,0 ) ) * float2( -1,1 ) );
				float2 temp_output_16_0_g42 = ( frac( ( temp_output_11_0_g42 - 0.5 ) ) * temp_output_15_0_g42 );
				float2 temp_output_19_0_g42 = ( temp_output_21_0_g42 + temp_output_16_0_g42 );
				float2 temp_output_51_0_g42 = ( frac( temp_output_19_0_g42 ) + float2( 0.5,0.5 ) );
				float temp_output_2_0_g45 = temp_output_21_0_g42.x;
				float2 temp_output_6_0_g45 = (float2( 1024,1024 )).xy;
				float2 temp_output_8_0_g45 = ( ddx( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult9_g45 = dot( temp_output_8_0_g45 , temp_output_8_0_g45 );
				float2 temp_output_7_0_g45 = ( ddy( temp_output_2_0_g45 ) * temp_output_6_0_g45 );
				float dotResult10_g45 = dot( temp_output_7_0_g45 , temp_output_7_0_g45 );
				float ifLocalVar11_g45 = 0;
				if( dotResult9_g45 > dotResult10_g45 )
				ifLocalVar11_g45 = dotResult9_g45;
				else if( dotResult9_g45 < dotResult10_g45 )
				ifLocalVar11_g45 = dotResult10_g45;
				float temp_output_26_0_g42 = ( log2( sqrt( ifLocalVar11_g45 ) ) + _MipBias );
				float4 tex2DNode49_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar88_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar88_g42 = tex2D( _FlowDiffuse, temp_output_51_0_g42 );
				else
				ifLocalVar88_g42 = tex2DNode49_g42;
				float temp_output_12_0_g42 = frac( temp_output_11_0_g42 );
				float2 temp_output_13_0_g42 = ( temp_output_15_0_g42 * temp_output_12_0_g42 );
				float2 temp_output_22_0_g42 = ( temp_output_13_0_g42 + temp_output_21_0_g42 );
				float4 tex2DNode32_g42 = tex2Dlod( _FlowDiffuse, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar83_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar83_g42 = tex2D( _FlowDiffuse, temp_output_22_0_g42 );
				else
				ifLocalVar83_g42 = tex2DNode32_g42;
				float temp_output_2_0_g46 = temp_output_12_0_g42;
				float temp_output_3_0_g46 = 1.0;
				float temp_output_4_0_g46 = ( temp_output_2_0_g46 + ( 0.25 * temp_output_3_0_g46 ) );
				float ifLocalVar24_g46 = 0;
				if( 0 == 0.0 )
				ifLocalVar24_g46 = temp_output_2_0_g46;
				else
				ifLocalVar24_g46 = temp_output_4_0_g46;
				float temp_output_7_0_g46 = frac( ( ifLocalVar24_g46 / temp_output_3_0_g46 ) );
				float temp_output_8_0_g46 = ( 2.0 * temp_output_7_0_g46 );
				float temp_output_12_0_g46 = floor( temp_output_8_0_g46 );
				float lerpResult13_g46 = lerp( temp_output_8_0_g46 , ( 2.0 * ( 1.0 - temp_output_7_0_g46 ) ) , temp_output_12_0_g46);
				float temp_output_78_0_g42 = lerpResult13_g46;
				float4 lerpResult69_g42 = lerp( ifLocalVar88_g42 , ifLocalVar83_g42 , temp_output_78_0_g42);
				float3 temp_output_57_72 = (lerpResult69_g42).rgb;
				
				float4 tex2DNode48_g42 = tex2Dlod( _FlowNormal, float4( temp_output_51_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar89_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar89_g42 = tex2D( _FlowNormal, temp_output_51_0_g42 );
				else
				ifLocalVar89_g42 = tex2DNode48_g42;
				float4 tex2DNode34_g42 = tex2Dlod( _FlowNormal, float4( temp_output_22_0_g42, 0, temp_output_26_0_g42) );
				float4 ifLocalVar84_g42 = 0;
				if( temp_output_90_0_g42 == 0.0 )
				ifLocalVar84_g42 = tex2D( _FlowNormal, temp_output_22_0_g42 );
				else
				ifLocalVar84_g42 = tex2DNode34_g42;
				float4 lerpResult68_g42 = lerp( ifLocalVar89_g42 , ifLocalVar84_g42 , temp_output_78_0_g42);
				
				float4 ase_screenPosNorm = ScreenPos / ScreenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen56 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither56 = Dither4x4Bayer( fmod(clipScreen56.x, 4), fmod(clipScreen56.y, 4) );
				float temp_output_1_0_g50 = _NoiseContrast;
				float2 texCoord23 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float lerpResult17_g4 = lerp( 1.0 , 0.01 , _Hardness);
				#ifdef _VERTEXCOLORSPHEREMASK_ON
				float staticSwitch15 = IN.ase_color.r;
				#else
				float staticSwitch15 = pow( ( 1.0 - saturate( ( distance( texCoord23 , float2( 0.5,0.5 ) ) / _Radius ) ) ) , lerpResult17_g4 );
				#endif
				float lerpResult5_g50 = lerp( ( 0.0 - temp_output_1_0_g50 ) , ( temp_output_1_0_g50 + 1.0 ) , ( temp_output_57_72 * staticSwitch15 ).x);
				dither56 = step( dither56, ( IN.ase_color.a * saturate( lerpResult5_g50 ) ) );
				
				float3 Albedo = ( float4( temp_output_57_72 , 0.0 ) * IN.ase_color ).rgb;
				float3 Normal = lerpResult68_g42.rgb;
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = 0;
				float Smoothness = 0.5;
				float Occlusion = 1;
				float Alpha = dither56;
				float AlphaClipThreshold = _AlphaClipThreshold;
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
300;73;1124;611;194.8138;225.5178;1;True;False
Node;AmplifyShaderEditor.RangedFloatNode;43;-2784,-16;Inherit;False;Constant;_Float1;Float 1;10;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;51;-2896,416;Inherit;False;Property;_RotateSpeed;Rotate Speed;6;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;52;-2896,336;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;38;-3119.988,-581.6906;Inherit;False;Property;_ParticleRandomValue;Particle Random Value;15;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;45;-3161.759,-167.3269;Inherit;False;Property;_ParticleRelativeTime;Particle Relative Time;17;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;44;-2816,64;Inherit;False;Property;_NoiseSpeed;Noise Speed;4;0;Create;True;0;0;0;False;0;False;0;-4.3515;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;50;-2688,368;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;41;-2704,-160;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;40;-2816,-288;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;42;-2560,-64;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;37;-2288,-496;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;12;False;1;FLOAT;0
Node;AmplifyShaderEditor.PannerNode;39;-2368,-288;Inherit;False;3;0;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;55;-2560,368;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.25;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;53;-2640,240;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;36;-2157.782,-235.6875;Inherit;False;Property;_NoiseScale;Noise Scale;3;0;Create;True;0;0;0;False;0;False;1;0.638033;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;35;-2112,-416;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RotatorNode;49;-2384,272;Inherit;False;3;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;2;FLOAT;1;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;48;-2432,0;Inherit;True;Property;_FlowMap;Flow Map;9;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;cf400388acc294e4bad75c383cf92556;cf400388acc294e4bad75c383cf92556;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;34;-1968,-352;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;47;-2128,80;Inherit;True;Property;_TextureSample9;Texture Sample 9;13;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;32;-1840,-208;Inherit;False;True;True;False;False;1;0;COLOR;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ComponentMaskNode;33;-1840,-352;Inherit;False;True;True;False;False;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;20;-1457.254,501.2129;Inherit;False;Property;_Hardness;Hardness;0;0;Create;True;0;0;0;False;0;False;0.1889;-0.6;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;31;-1600,-320;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;23;-1505.254,293.2129;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;19;-1441.254,421.2129;Inherit;False;Property;_Radius;Radius;5;0;Create;True;0;0;0;False;0;False;0.5;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;25;-1102.782,-74.47652;Inherit;False;Property;_MipBias;Mip Bias;1;0;Create;True;0;0;0;False;0;False;1;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;16;-1188.493,91.27739;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleDivideOpNode;30;-1440,-320;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;2,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TexturePropertyNode;28;-1232,-672;Inherit;True;Property;_FlowDiffuse;Flow Diffuse;8;1;[NoScaleOffset];Create;True;0;0;0;False;0;False;26108abb549a6c14b9d8b4202bd1af55;26108abb549a6c14b9d8b4202bd1af55;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.TextureCoordinatesNode;27;-1236,-198;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;29;-1232,-480;Inherit;True;Property;_FlowNormal;Flow Normal;10;2;[NoScaleOffset];[Normal];Create;True;0;0;0;False;0;False;19d9b8e13eb43134ebfd13159334e9c9;19d9b8e13eb43134ebfd13159334e9c9;True;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.FunctionNode;22;-1207.645,368.4873;Inherit;False;SF_CircularGradient;-1;;4;d3fc10aa8ab264846b4a7af6eb562caa;0;4;1;FLOAT2;0,0;False;2;FLOAT2;0.5,0.5;False;3;FLOAT;0;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;57;-918.1517,-363.5578;Inherit;False;SH_UEFlowmaps;11;;42;2bacb256711f8e747822b1b1e7e19f16;0;9;46;SAMPLER2D;;False;47;SAMPLER2D;;False;14;FLOAT2;0.1,0;False;52;FLOAT2;0.5,0.5;False;11;FLOAT;0;False;90;INT;1;False;21;FLOAT2;0,0;False;25;FLOAT2;1024,1024;False;27;FLOAT;0;False;5;FLOAT3;72;FLOAT;73;COLOR;67;FLOAT4;63;FLOAT2;62
Node;AmplifyShaderEditor.StaticSwitch;15;-813.093,246.2774;Inherit;False;Property;_VertexColorSphereMask;Vertex Color / Sphere Mask;7;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;14;-398.2539,130.2129;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;13;-561.2539,374.2129;Inherit;False;Property;_NoiseContrast;Noise Contrast;2;0;Create;True;0;0;0;False;0;False;0.5;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;9;-113.2539,-26.7871;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;12;-177.2539,197.2129;Inherit;True;SF_UECheapContrast;-1;;50;93d366414e6f1cc45b1077ffab365271;0;2;2;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;10;222.7461,85.21289;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;61;-1382.245,-47.97327;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;8;200.9579,-366.3671;Inherit;True;2;2;0;FLOAT3;0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.DitheringNode;56;464,80;Inherit;False;0;False;4;0;FLOAT;0;False;1;SAMPLER2D;;False;2;FLOAT4;0,0,0,0;False;3;SAMPLERSTATE;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;59;-1471.367,-199.6227;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;11;432,160;Inherit;False;Property;_AlphaClipThreshold;Alpha Clip Threshold;18;0;Create;True;0;0;0;False;0;False;0.3333;0.314;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;60;-1723.367,-75.62268;Inherit;False;Property;_Speed;Speed;16;0;Create;True;0;0;0;False;0;False;0;0.073;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;GBuffer;0;7;GBuffer;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalGBuffer;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthNormals;0;6;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=DepthNormals;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;800,-64;Float;False;True;-1;2;UnityEditor.ShaderGraphLitGUI;0;2;SH_Waterfall_Sprite;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;18;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;38;Workflow;1;0;Surface;0;0;  Refraction Model;0;0;  Blend;0;0;Two Sided;1;0;Fragment Normal Space,InvertActionOnDeselection;0;0;Transmission;0;0;  Transmission Shadow;0.5,False,-1;0;Translucency;0;0;  Translucency Strength;1,False,-1;0;  Normal Distortion;0.5,False,-1;0;  Scattering;2,False,-1;0;  Direct;0.9,False,-1;0;  Ambient;0.1,False,-1;0;  Shadow;0.5,False,-1;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;GPU Instancing;1;0;LOD CrossFade;1;0;Built-in Fog;1;0;_FinalColorxAlpha;0;0;Meta Pass;1;0;Override Baked GI;0;0;Extra Pre Pass;0;0;DOTS Instancing;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,-1;0;  Type;0;0;  Tess;16,False,-1;0;  Min;10,False,-1;0;  Max;25,False,-1;0;  Edge Length;16,False,-1;0;  Max Displacement;25,False,-1;0;Write Depth;0;0;  Early Z;0;0;Vertex Position,InvertActionOnDeselection;1;0;0;8;False;True;True;True;True;True;True;True;False;;False;0
WireConnection;50;0;52;0
WireConnection;50;1;51;0
WireConnection;41;0;45;0
WireConnection;41;1;38;0
WireConnection;42;0;43;0
WireConnection;42;1;44;0
WireConnection;37;0;38;0
WireConnection;39;0;40;0
WireConnection;39;2;42;0
WireConnection;39;1;41;0
WireConnection;55;0;50;0
WireConnection;35;0;37;0
WireConnection;35;1;39;0
WireConnection;49;0;53;0
WireConnection;49;2;55;0
WireConnection;34;0;35;0
WireConnection;34;1;36;0
WireConnection;47;0;48;0
WireConnection;47;1;49;0
WireConnection;32;0;47;0
WireConnection;33;0;34;0
WireConnection;31;0;33;0
WireConnection;31;1;32;0
WireConnection;30;0;31;0
WireConnection;22;1;23;0
WireConnection;22;3;19;0
WireConnection;22;4;20;0
WireConnection;57;46;28;0
WireConnection;57;47;29;0
WireConnection;57;14;30;0
WireConnection;57;21;27;0
WireConnection;57;27;25;0
WireConnection;15;1;22;0
WireConnection;15;0;16;1
WireConnection;14;0;57;72
WireConnection;14;1;15;0
WireConnection;12;2;14;0
WireConnection;12;1;13;0
WireConnection;10;0;9;4
WireConnection;10;1;12;0
WireConnection;61;0;59;0
WireConnection;61;1;59;0
WireConnection;8;0;57;72
WireConnection;8;1;9;0
WireConnection;56;0;10;0
WireConnection;59;0;60;0
WireConnection;1;0;8;0
WireConnection;1;1;57;67
WireConnection;1;6;56;0
WireConnection;1;7;11;0
ASEEND*/
//CHKSM=35026F4A89AFE9C82A9A4021F75906763B8F431D