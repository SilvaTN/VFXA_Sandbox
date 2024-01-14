using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.InputSystem;

public class InteractiveDisplay : MonoBehaviour
{
    public Transform spawnPoint;
    public GameObject Effect;
    public float lifetime;
    public Key triggerKey;

    private GameObject myCurrentEffect;
    void Start() {

    }

    void Update() {
        if (Keyboard.current[triggerKey].wasPressedThisFrame) {
            if (Effect != null) {
                if (myCurrentEffect != null) {
                    Destroy(myCurrentEffect.gameObject);
                }
                myCurrentEffect = Instantiate(Effect, spawnPoint.position, spawnPoint.rotation);
                if (lifetime > 0) {
                    StartCoroutine(KillEffectAfterLifetime());
                }
            } else {
                Debug.Log("No Effect GameObject attached to the Interactive Display!");
            }
        }
    }

    IEnumerator KillEffectAfterLifetime() {
        yield return new WaitForSeconds(lifetime);
        Destroy(myCurrentEffect.gameObject);
    }
}
