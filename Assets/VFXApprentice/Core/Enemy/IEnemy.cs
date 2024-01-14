using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface IEnemy
{
    public void DealDamage(float intensity, Vector3 direction);
    public Vector3 GetImpactVFXPosition();
}
