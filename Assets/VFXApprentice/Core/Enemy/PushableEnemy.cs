using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[AddComponentMenu("Enemy/Pushable")]
public class PushableEnemy : MonoBehaviour, IEnemy
{
    public void DealDamage(float intensity, Vector3 direction) {
        print(this.name + " received " + intensity + " damage");
        this.GetComponent<Rigidbody>().AddExplosionForce(intensity * 500, this.transform.position - direction, 100f);
        //Destroy(this.gameObject);
    }

    public Vector3 GetImpactVFXPosition() {
        return Vector3.zero;
    }
}
