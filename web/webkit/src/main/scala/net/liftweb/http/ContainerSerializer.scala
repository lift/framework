package net.liftweb.http

/**
 * A trait that provides *actual* serialization of a type so that
 * the type can be stored into a container's session and be migrated across
 * servers
 */
trait ContainerSerializer[T] extends Serializable {
  def serialize(in: T): Array[Byte]

  def deserialize(in: Array[Byte]): T
}

/**
  * A trait that serializes regardless of type. Sadly in Java land, most implementations
  * will look like this.
  */
trait UntypedContainerSerializer extends Serializable { self =>
  def serialize(in: Any): Array[Byte]

  def deserialize(in: Array[Byte]): Any

  /**
    * Convert into a typed ContainerSerializer[T], leaning on asInstanceOf[T] in hopes for the best
    * @tparam T the type of object to serialize
    */
  def typed[T]: ContainerSerializer[T] = new ContainerSerializer[T] {
    override def serialize(in: T): Array[Byte] = self.serialize(in)

    override def deserialize(in: Array[Byte]): T = self.deserialize(in).asInstanceOf[T]
  }
}

object ContainerSerializer {

  import java.util.Date
  import org.joda.time.DateTime

  /**
    * The default java serialization machinery
    */
  def javaSerializer: UntypedContainerSerializer =
    new UntypedContainerSerializer {

      import java.io._

      override def serialize(in: Any): Array[Byte] = {
        val bos = new ByteArrayOutputStream()
        val oos = new ObjectOutputStream(bos)
        oos.writeObject(in)
        oos.flush()
        bos.toByteArray()
      }

      override def deserialize(in: Array[Byte]): Any = {
        val bis = new ByteArrayInputStream(in)
        val ois = new ObjectInputStream(bis)
        ois.readObject
      }
    }

  private def buildSerializer[T]: ContainerSerializer[T] = LiftRules.lockedContainerSerializer.typed

  implicit lazy val objectSerializer: ContainerSerializer[Object] = buildSerializer
  implicit lazy val intSerializer: ContainerSerializer[Int] = buildSerializer
  implicit lazy val longSerializer: ContainerSerializer[Long] = buildSerializer
  implicit lazy val charSerializer: ContainerSerializer[Char] = buildSerializer
  implicit lazy val shortSerializer: ContainerSerializer[Short] = buildSerializer
  implicit lazy val byteSerializer: ContainerSerializer[Byte] = buildSerializer
  implicit lazy val floatSerializer: ContainerSerializer[Float] = buildSerializer
  implicit lazy val doubleSerializer: ContainerSerializer[Double] = buildSerializer
  implicit lazy val booleanSerializer: ContainerSerializer[Boolean] = buildSerializer
  implicit lazy val dateSerializer: ContainerSerializer[Date] = buildSerializer
  implicit lazy val stringSerializer: ContainerSerializer[String] = buildSerializer
  implicit lazy val jodaDateSerializer: ContainerSerializer[DateTime] = buildSerializer

  implicit def arraySerializer[T](implicit tc: ContainerSerializer[T]): ContainerSerializer[Array[T]] = buildSerializer

  implicit def listSerializer[T](implicit tc: ContainerSerializer[T]): ContainerSerializer[List[T]] = buildSerializer

  implicit def anyRefSerializer[T <: Serializable]: ContainerSerializer[T] = buildSerializer
}