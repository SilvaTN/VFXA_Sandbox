using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class InstantSpell : Spell
{
    [Header("Spell VFX")]
    public GameObject windUpVFX;
    public Transform windUpVFXSocket;
    public GameObject castVFX;
    public GameObject areaVFX;
    [Header("Spell Animations")]
    public bool bLookAtCursorWhileHolding = true;
    [Header("Spell Area")]
    public GameObject impactVFX;
    public float impactCollisionRadius;
    public float damageIntensity;

    private GameObject windUpGameObject;
    private SphereCollider hurtbox;

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
        owner.animator.SetBool("castingInstantSpell", true);
        owner.animator.Play("SummonIn");
        //Make the caster immobile
        owner.SetCharacterMobility(false, false);
    }
    public override void Release() {
        base.Release();
        //Jump to the "Cast" animation
        owner.animator.SetBool("castingInstantSpell", false);
        //Selfdestruct after a small delay
        StartCoroutine(Waiter.WaitAndDo(EndSpell, 1f));
    }

    public override void CustomSpellEvent(string code) {
        base.CustomSpellEvent(code);
        if (code == "WindUpVFX") {
            SpawnWindUpVFX();
        }
        if(code == "AreaVFX") {
            RemoveWindUpVFX();
            SpawnCastVFX();
            SpawnAreaVFX();
        }
    }

    private void SpawnWindUpVFX() {
        windUpGameObject = Instantiate(windUpVFX, windUpVFXSocket);
    }
    private void RemoveWindUpVFX() {
        Destroy(windUpGameObject);
    }
    private void SpawnCastVFX() {
        Instantiate(castVFX, windUpVFXSocket.position, Quaternion.identity);
    }
    private void SpawnAreaVFX() {
        //Get the cursor position
        Vector3 areaPosition = owner.GetMouseWorldPosition();
        //Spawn VFX
        Instantiate(areaVFX, areaPosition, Quaternion.identity);
        //Set Collider at position
        this.transform.position = areaPosition;
        hurtbox = this.gameObject.AddComponent<SphereCollider>();
        hurtbox.isTrigger = true;
        hurtbox.radius = impactCollisionRadius;
    }

    private void EndSpell() {
        //Let the caster move
        owner.SetCharacterMobility(true, true);
        //Destroy the spell
        Destroy(this.gameObject);
    }

    //UTILS
    public void LoadInstantSpellValues(InstantSpellValues values) {
        this.name = values.spellName;
        this.gameObject.name = values.spellName;
        this.windUpVFX = values.windUpVFX;
        this.windUpVFXSocket = values.windUpVFXSocket;
        this.castVFX = values.castVFX;
        this.areaVFX = values.areaVFX;
        this.impactVFX = values.impactVFX;
        this.impactCollisionRadius = values.impactCollisionRadius;
        this.damageIntensity = values.damageIntensity;
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
}
