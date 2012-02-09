package lmxml
package example

case class Person(
  firstname: String,
  lastname: String,
  email: String = "",
  address: String = "",
  id: Int = 0) {
  def fullname() = "%s %s" format(firstname, lastname)
}

object Database {
  private var increment = 1

  private val people = collection.mutable.HashMap[Int, Person]()

  def save(person: Person) {
    if (person.id == 0) {
      people(increment) = person.copy(id = increment)
      increment += 1
    } else {
      people(person.id) = person
    }
  }

  def remove(id: Int) = people -= id

  def getAll() = people.values.toList

  def get(id: Int) = people.get(id)
}
