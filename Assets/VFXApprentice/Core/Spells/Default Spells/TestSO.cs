using UnityEngine;

[CreateAssetMenu(fileName = "TestSO", menuName = "SO")]
public class TestSO : ScriptableObject
{

    public string spellName;
    public float intensity;

    public void function1() {
        Debug.Log("function1");
    }
}
