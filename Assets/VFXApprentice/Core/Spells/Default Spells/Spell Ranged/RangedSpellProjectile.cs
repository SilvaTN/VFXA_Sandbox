using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RangedSpellProjectile : MonoBehaviour
{

    [Header("Projectile VFX")]
    public GameObject projectileHeadVFX;
    public GameObject projectileTrailVFX;
    public GameObject impactVFX;

    [Header("Projectile")]
    public float projectileSpeed;
    public float projectileMaxLifetime;
    public float projectileMaxRange;
    public bool bDestroyOnImpact = true;
    public float hurtboxRadius;
    public float damageIntensity;

    private SphereCollider col;
    private Vector3 startPosition;
    private Vector3 projectileVelocity;
    private float selfdestroyTimer;

    //UNITY METHODS
    public void Awake() {
        col = gameObject.AddComponent<SphereCollider>();
        col.isTrigger = true;
    }
    public void Start() {
        //Set the Collider size
        col.radius = hurtboxRadius;
        //Remember the initial location
        startPosition = this.transform.position;
        //Start the selfdestroyTimer
        selfdestroyTimer = projectileMaxLifetime;
        //Start Moving
        projectileVelocity = projectileSpeed * transform.forward;
        //TODO - Spawn the Projectile VFX - Needed with the new system?
    }
    public void Update() {
        //If there is a max range
        if (projectileMaxRange > 0) {
            //Check if projectile reached its max range
            float d = Vector3.Distance(startPosition, this.transform.position);
            if (d > projectileMaxRange) {
                Destroy(this.gameObject);
            }
        }
    }
    public void FixedUpdate() {
        //If there is a max lifetime
        if (projectileMaxLifetime > 0) {
            //Check selfdestroy timer
            selfdestroyTimer -= Time.fixedDeltaTime;
            if (selfdestroyTimer <= 0) {
                Destroy(this.gameObject);
            }
        }
        //Update the missile position depending on the velocity
        this.transform.position += projectileVelocity * Time.fixedDeltaTime;
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
            Instantiate(impactVFX, this.transform.position, Quaternion.identity);
            //if required, destroy self
            if (bDestroyOnImpact) {
                Destroy(this.gameObject);
            }
        }
    }
}
