using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class InstantSpellArea : MonoBehaviour
{
    [Header("Area VFX")]
    public GameObject ImpactVFX;

    [Header("Area")]
    public float lifetime;
    public float impactCollisionRadius;
    public float damageIntensity;

    private SphereCollider hurtbox;

    private void Start() {
        hurtbox = this.gameObject.AddComponent<SphereCollider>();
        hurtbox.isTrigger = true;
        hurtbox.radius = impactCollisionRadius;
        Waiter.WaitAndDo(EndArea, lifetime);
    }

    private void EndArea() {
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
            Instantiate(ImpactVFX, other.transform.position, Quaternion.identity);
        }
    }

}
