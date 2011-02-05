import junit.framework._;
import Assert._;

import net.liftweb.ldap.SimpleLDAPVendor

object LDAPTest {
    def suite: Test = new TestSuite(classOf[LDAPTest])

    def main(args : Array[String]) = junit.textui.TestRunner.run(suite)
}

class LDAPTest extends TestCase("ldap") {
    def testParameters = {
        SimpleLDAPVendor.parameters = () => Map("ldap.url" -> "localhost")

        assertTrue(SimpleLDAPVendor.parameters().get("ldap.url") == Some("localhost"))
    }

    def testLoadParametersFile = {
        SimpleLDAPVendor.parameters = () => SimpleLDAPVendor.parametersFromFile("src/test/resources/ldap.properties")

        assertTrue(SimpleLDAPVendor.parameters().get("ldap.url") == Some("ldap://localhost"))
    }
}
