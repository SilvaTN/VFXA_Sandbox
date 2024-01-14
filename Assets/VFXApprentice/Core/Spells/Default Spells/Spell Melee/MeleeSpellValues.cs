using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class MeleeSpellValues
{
    [Tooltip("Shown in the UI when the spell is selected")]
    public string spellName = "New Melee Spell";
    public GameObject SlashVFX;
    public GameObject impactVFX;
    public float hurtboxRange;
    public float hurtboxWidth;
    public float hurtboxHeight;
    public float hurtboxLifetime;
    [Tooltip("The intensity with which dummies or destructibles react to the spell")]
    public float damageIntensity = 1;
}
