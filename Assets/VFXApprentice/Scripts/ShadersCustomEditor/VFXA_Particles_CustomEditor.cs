using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class VFXA_Particles_CustomEditor : ShaderGUI
{
    bool showEmissive = true;
    bool showTextureOne = true;
    bool showTextureTwo = true;
    bool showMask = true;
    bool showDissolve = true;
    bool showVertexOffset = true;
    bool showRenderingOptions = true;

    override public void OnGUI(MaterialEditor materialEditor, MaterialProperty[] properties)
    {
        EditorGUILayout.HelpBox("Values driven by Particle System \n\n " +
            "   CUSTOM 1 \n" +
            "----------------- \n" +
            " X: Dissolve Amount\n" +
            " Y: Dissolve Smoothness \n" +
            " Z: Intensity \n", MessageType.Info);

        #region Rendering
        showRenderingOptions = EditorGUILayout.BeginFoldoutHeaderGroup(showRenderingOptions, "Rendering Options");
        if (showRenderingOptions)
        {
            MaterialProperty CullMode = ShaderGUI.FindProperty("_CullMode", properties);
            MaterialProperty WriteDepth = ShaderGUI.FindProperty("_WriteDepth", properties);
            MaterialProperty ZTest = ShaderGUI.FindProperty("_ZTest", properties);


            materialEditor.ShaderProperty(CullMode, "Cull Mode");
            materialEditor.ShaderProperty(WriteDepth, "WriteDepth");
            materialEditor.ShaderProperty(ZTest, "ZTest");

        }
        EditorGUILayout.EndFoldoutHeaderGroup();
        #endregion

        #region Emissive
        showEmissive = EditorGUILayout.BeginFoldoutHeaderGroup(showTextureOne, "Emissive");
        if(showEmissive){
            MaterialProperty Emissive_Strength = ShaderGUI.FindProperty("_EmissiveStrength", properties);
            materialEditor.FloatProperty(Emissive_Strength,"Emissive Strength");
        }
        EditorGUILayout.EndFoldoutHeaderGroup();
        #endregion

        #region Texture 01
        showTextureOne = EditorGUILayout.BeginFoldoutHeaderGroup(showTextureOne, "Texture 01");

        if (showTextureOne)
        {
            MaterialProperty Texture_01 = ShaderGUI.FindProperty("_Texture_01", properties);
            MaterialProperty Pan_01_X = ShaderGUI.FindProperty("_Pan_01_X", properties);
            MaterialProperty Pan_01_Y = ShaderGUI.FindProperty("_Pan_01_Y", properties);


            materialEditor.TextureProperty(Texture_01, "", true);
            materialEditor.ShaderProperty(Pan_01_X, "Panner X");
            materialEditor.ShaderProperty(Pan_01_Y, "Panner Y");

        }

        EditorGUILayout.EndFoldoutHeaderGroup();
        #endregion

        EditorGUILayout.Space();

        #region Texture 02
        showTextureTwo = EditorGUILayout.BeginFoldoutHeaderGroup(showTextureTwo, "Texture 02");

        if (showTextureTwo)
        {
            MaterialProperty Texture_02 = ShaderGUI.FindProperty("_Texture_02", properties);
            MaterialProperty Pan_02_X = ShaderGUI.FindProperty("_Pan_02_X", properties);
            MaterialProperty Pan_02_Y = ShaderGUI.FindProperty("_Pan_02_Y", properties);


            materialEditor.TextureProperty(Texture_02, "");
            materialEditor.ShaderProperty(Pan_02_X, "Panner X");
            materialEditor.ShaderProperty(Pan_02_Y, "Panner Y");

        }

        EditorGUILayout.EndFoldoutHeaderGroup();
        #endregion

        EditorGUILayout.Space();

        #region Mask
        showMask = EditorGUILayout.BeginFoldoutHeaderGroup(showMask, "Mask");

        if (showMask)
        {
            MaterialProperty Mask = ShaderGUI.FindProperty("_Mask", properties);

            materialEditor.TextureProperty(Mask, "");
        }

        EditorGUILayout.EndFoldoutHeaderGroup();
        #endregion

        #region Dissolve
        showDissolve = EditorGUILayout.BeginFoldoutHeaderGroup(showDissolve, "Dissolve Map");

        if (showDissolve)
        {
            MaterialProperty DissolveMap = ShaderGUI.FindProperty("_DissolveMap", properties);
            MaterialProperty Pan_03_X = ShaderGUI.FindProperty("_Pan_03_X", properties);
            MaterialProperty Pan_03_Y = ShaderGUI.FindProperty("_Pan_03_Y", properties);

            materialEditor.TextureProperty(DissolveMap, "");
            materialEditor.ShaderProperty(Pan_03_X, "Panner X");
            materialEditor.ShaderProperty(Pan_03_Y, "Panner Y");
        }

        EditorGUILayout.EndFoldoutHeaderGroup();
        #endregion

        #region VertexOffset
        showVertexOffset = EditorGUILayout.BeginFoldoutHeaderGroup(showVertexOffset, "VertexOffset");

        if (showVertexOffset)
        {
            MaterialProperty VertexOffsetTexture = ShaderGUI.FindProperty("_VertexOffsetMap", properties);            
            MaterialProperty VertexOffsetMask = ShaderGUI.FindProperty("_VertexOffsetMask", properties);
            MaterialProperty Pan_04_X = ShaderGUI.FindProperty("_Pan_04_X", properties);
            MaterialProperty Pan_04_Y = ShaderGUI.FindProperty("_Pan_04_Y", properties);
            MaterialProperty OffsetAmount = ShaderGUI.FindProperty("_OffsetAmount", properties);
            MaterialProperty VertexOffsetTiling = ShaderGUI.FindProperty("_VertexOffsetTiling", properties);

            materialEditor.TextureProperty(VertexOffsetTexture, "Vertex Offset Texture");
            materialEditor.TextureProperty(VertexOffsetMask, "Vertex Offset Mask");
            materialEditor.ShaderProperty(Pan_04_X, "Panner X");
            materialEditor.ShaderProperty(Pan_04_Y, "Panner Y");
            materialEditor.ShaderProperty(OffsetAmount, "Offset Amount");
            materialEditor.ShaderProperty(VertexOffsetTiling, "Vertex Offset Tiling");
        }

        EditorGUILayout.EndFoldoutHeaderGroup();
        #endregion
    }

}
