using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TurboSlash : Spell
{
    [Header("Spell VFX")]
    public GameObject slashVFX;
    public GameObject impactVFX;
    [Header("Spell Hurtbox")]
    public float hurtboxRange;
    public float hurtboxWidth;
    public float hurtboxHeight;
    public float hurtboxLifetime;
    public float damageIntensity = 1;

    //SPELL EVENTS
    public override void Cast() {
        base.Cast();
        //Look at cursor
        owner.LookAtCursor();
        //Play the spell animation
        owner.animator.SetBool("castingMeleeSpell", true);
        //Make the caster immobile
        owner.SetCharacterMobility(false, false);
    }
    public override void Release() {
        base.Release();
        //Jump to the "Cast" animation
        owner.animator.SetBool("castingMeleeSpell", false);
    }
    public override void CustomSpellEvent(string code) {
        base.CustomSpellEvent(code);
        if(code == "SlashVFX") {
            SpawnSlashVFX();
        }
        if(code == "DealDamage")  {
            //Deal Damage
            DealDamage();
            //Auomatically release the spell if the player hasn't already
            ForceRelease();
        }
    }

    private void SpawnSlashVFX() {
        GameObject slashGameObject = Instantiate(slashVFX);
        slashGameObject.transform.position = this.owner.transform.position;
        slashGameObject.transform.rotation = this.owner.transform.rotation;
    }
    private void DealDamage() {
        //TODO
    }
    private void ForceRelease() {
        //TODO - BUG!!!!!
        this.Release();
        //Selfdestruct after a small delay
        StartCoroutine(Waiter.WaitAndDo(EndSpell, 0.5f));
    }
    private void EndSpell() {
        //Let the caster move
        owner.SetCharacterMobility(true, true);
        //Destroy the spell
        Destroy(this.gameObject);
    }

    //UTILS
    public void LoadMeleeSpellValues(MeleeSpellValues values) {
        this.name = values.spellName;
        this.gameObject.name = values.spellName;
        this.slashVFX = values.SlashVFX;
        this.impactVFX = values.impactVFX;
        this.hurtboxRange = values.hurtboxRange;
        this.hurtboxWidth = values.hurtboxWidth;
        this.hurtboxHeight = values.hurtboxHeight;
        this.hurtboxLifetime = values.hurtboxLifetime;
        this.damageIntensity = values.damageIntensity;
    }
}
