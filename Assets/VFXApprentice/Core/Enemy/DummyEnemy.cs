using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DummyEnemy : MonoBehaviour, IEnemy
{
    public Transform impactVFXSocket;

    public void DealDamage(float intensity, Vector3 direction) {
        print(this.name + " received " + intensity + " damage");
    }

    public Vector3 GetImpactVFXPosition() {
        //TODO - Enum dropdown in the paramters that lets decide which transform to use? (Head, chest...)
        return impactVFXSocket.transform.position - this.transform.position;
    }
}
