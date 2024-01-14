using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RangedSpell : Spell
{
    [Header("Spell VFX")]
    public GameObject windUpVFX;
    public Transform windUpVFXSocket;
    public GameObject castVFX;
    public GameObject projectileHeadVFX;
    public GameObject projectileTrailVFX;
    public GameObject impactVFX;
    [Header("Spell Animations")]
    public bool bLookAtCursorWhileHolding = true;
    [Header("Spell Projectile")]
    public GameObject projectileGameObject = null;

    private GameObject windUpVFXRef;

    //UNITY METHODS
    new public void Start() {
        base.Start();
    }
    new public void Update() {
        base.Update();
        if (bLookAtCursorWhileHolding) {
            if (bSpellCast && !bSpellReleased) {
                //Look at cursor
                owner.LookAtCursor();
            }
        }
    }

    //SPELL EVENTS
    public override void Cast() {
        base.Cast();
        //Look at cursor
        owner.LookAtCursor();
        //Play the spell animation
        owner.animator.SetBool("castingRangedSpell", true);
        owner.animator.Play("CastForwardIn");
        //Make the caster immobile
        owner.SetCharacterMobility(false, false);
    }
    public override void Release() {
        base.Release();
        //Jump to the "Cast" animation
        owner.animator.SetBool("castingRangedSpell", false);
        //Selfdestruct after a small delay
        StartCoroutine(Waiter.WaitAndDo(EndSpell, 0.5f));
    }
    public override void CustomSpellEvent(string code) {
        base.CustomSpellEvent(code);
        if (code == "StartWindUp") {
            StartWindUpVFX();
        }
        if (code == "SpawnProjectile") {
            SpawnProjectile();
            RemoveWindUpVFX();
        }
    }

    private void StartWindUpVFX() {
        //Attach spell to character in the desired transform (Needed?)

        //Spawn the WindUp VFX attached to the desired transform
        windUpVFXRef = Instantiate(windUpVFX, windUpVFXSocket);
    }
    private void SpawnProjectile() {
        //Spawn the Firebal Projectile
        GameObject projectile = Instantiate(projectileGameObject, owner.transform.position + Vector3.up, owner.transform.rotation);
    }
    private void RemoveWindUpVFX() {
        //Remove the WindUp VFX
        Destroy(windUpVFXRef);
    }
    private void EndSpell() {
        //Let the caster move
        owner.SetCharacterMobility(true, true);
        //Destroy the spell
        Destroy(this.gameObject);
    }

    //UTILS
    public void LoadRangedSpellValues(RangedSpellValues values) {
        this.name = values.spellName;
        this.gameObject.name = values.spellName;
        this.windUpVFX = values.windUpVFX;
        this.windUpVFXSocket = values.windUpVFXSocket;
        this.castVFX = values.castVFX;
        this.projectileGameObject = values.projectileGameObject;
    }
}
