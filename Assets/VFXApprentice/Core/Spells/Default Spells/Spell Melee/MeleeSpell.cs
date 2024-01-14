using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MeleeSpell : Spell
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

    private BoxCollider hurtboxCollider;

    //SPELL EVENTS
    public override void Cast() {
        base.Cast();
        //Look at cursor
        owner.LookAtCursor();
        //Set up Hurtbox Collider
        SetupHurtbox();
        //Play the spell animation
        owner.animator.SetBool("castingMeleeSpell", true);
        owner.animator.Play("SlashOverhead");
        //Make the caster immobile
        owner.SetCharacterMobility(false, false);
    }
    public override void Release() {
        base.Release();
        //Jump to the "Cast" animation
        owner.animator.SetBool("castingMeleeSpell", false);
        //TODO - This is a hack
        StartCoroutine(Waiter.WaitAndDo(EndSpell, 1f));
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
    private void ForceRelease() {
        //TODO - BUG!!!!!
        this.Release();
        //Selfdestruct after a small delay
        StartCoroutine(Waiter.WaitAndDo(EndSpell, 0.5f));
    }
    
    //PRIVATE SPELL METHODS
    private void SpawnSlashVFX() {
        GameObject slashGameObject = Instantiate(slashVFX);
        slashGameObject.transform.position = this.owner.transform.position;
        slashGameObject.transform.rotation = this.owner.transform.rotation;
    }
    private void SetupHurtbox() {
        //Add hurtbox
        hurtboxCollider = this.gameObject.AddComponent<BoxCollider>();
        hurtboxCollider.isTrigger = true;
        hurtboxCollider.enabled = false;
        //Set Hurtbox Size
        hurtboxCollider.size = new(hurtboxWidth, hurtboxHeight, hurtboxRange);
        //Set Hurtbox Location
        hurtboxCollider.center = new(0f, 1f, hurtboxRange / 1.5f);
        this.transform.position = owner.transform.position;
        this.transform.rotation = owner.transform.rotation;
    }
    private void DealDamage() {
        //Enable hurtbox collider
        hurtboxCollider.enabled = true;
        //Set the hurtbox active lifetime
        StartCoroutine(Waiter.WaitAndDo(DisableHurtbox, hurtboxLifetime));
        //Start the EndSpell countdown
        StartCoroutine(Waiter.WaitAndDo(EndSpell, 0.5f));
    }
    private void DisableHurtbox() {
        hurtboxCollider.enabled = false;
    }
    private void EndSpell() {
        //Let the caster move
        owner.SetCharacterMobility(true, true);
        //Destroy the spell
        Destroy(this.gameObject);
    }

    //TRIGGERS & COLLISIONS
    public void OnTriggerEnter(Collider other) {
        //If the rigidbody detected is a type of enemy
        IEnemy enemy = other.GetComponent<IEnemy>();
        if (enemy != null) {
            //Deal the spell damage
            Vector3 dmgDirection = (other.transform.position - this.transform.position).normalized;
            enemy.DealDamage(damageIntensity, dmgDirection);
            //Spawn the impact VFX
            Instantiate(impactVFX, other.transform.position + enemy.GetImpactVFXPosition(), Quaternion.identity);
        }
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
