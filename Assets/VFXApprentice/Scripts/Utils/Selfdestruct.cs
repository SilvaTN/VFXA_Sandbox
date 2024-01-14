using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Selfdestruct : MonoBehaviour {

    /// <summary>
    /// The amount of time to wait after the Start() event before selfdestructing. If lifetime is equal or lower than zero the script won't selfdestruct on its own.
    /// </summary>
    [SerializeField]
    private float lifetime = 0f;

    /// <summary>
    /// All the dependents will be destroyed whenever the Kill() function is called.
    /// </summary>
    [SerializeField]
    private List<GameObject> dependents = null;

    private void Start() {
        if (lifetime > 0) {
            TimedKill(lifetime);
        }
    }

    /// <summary>
    /// Waits for the specified amount of time and destroys the GameObject containing this script.
    /// </summary>
    /// <param name="ttl">The amount of time to wait before selfdestructing</param>
    public void TimedKill(float ttl) {
        StartCoroutine(WaitAndKill(ttl));
    }

    private IEnumerator WaitAndKill(float t) {
        yield return new WaitForSeconds(t);
        Kill();
    }
    /// <summary>
    /// Destroys the GameObject containing this script and all the dependent GameObjects.
    /// </summary>
    public void Kill() {
        //Destroy all the dependent GameObjects
        if (dependents != null) {
            foreach(GameObject dependent in dependents) {
                Destroy(dependent);
            }
        }
        //Destroy self
        Destroy(this.gameObject);
    }
}