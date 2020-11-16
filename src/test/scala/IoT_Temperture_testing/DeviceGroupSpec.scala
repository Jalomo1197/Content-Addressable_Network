package IoT_Temperture_testing

import IoT_Temperture_example.Device.{Passivate, RecordTemperature, TemperatureRecorded}
import IoT_Temperture_example.DeviceManager.{DeviceRegistered, ReplyDeviceList, RequestDeviceList, RequestTrackDevice}
import IoT_Temperture_example._
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._

class DeviceGroupSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike {

  "be able to register a device actor" in {
    val probe = createTestProbe[DeviceRegistered]()
    val groupActor = spawn(DeviceGroup("group"))

    groupActor ! RequestTrackDevice("group", "device1", probe.ref)
    val registered1 = probe.receiveMessage()
    val deviceActor1 = registered1.device

    // another deviceId
    groupActor ! RequestTrackDevice("group", "device2", probe.ref)
    val registered2 = probe.receiveMessage()
    val deviceActor2 = registered2.device
    deviceActor1 should !==(deviceActor2)

    // Check that the device actors are working
    val recordProbe = createTestProbe[TemperatureRecorded]()
    deviceActor1 ! RecordTemperature(requestId = 0, 1.0, recordProbe.ref)
    recordProbe.expectMessage(TemperatureRecorded(requestId = 0))
    deviceActor2 ! Device.RecordTemperature(requestId = 1, 2.0, recordProbe.ref)
    recordProbe.expectMessage(Device.TemperatureRecorded(requestId = 1))
  }


  "ignore requests for wrong groupId" in {
    val probe = createTestProbe[DeviceRegistered]()
    val groupActor = spawn(DeviceGroup("group"))

    groupActor ! RequestTrackDevice("wrongGroup", "device1", probe.ref)
    probe.expectNoMessage(500.milliseconds)
  }


  "return same actor for same deviceId" in {
    val probe = createTestProbe[DeviceRegistered]()
    val groupActor = spawn(DeviceGroup("group"))

    groupActor ! RequestTrackDevice("group", "device1", probe.ref)
    val registered1 = probe.receiveMessage()

    // registering same again should be idempotent
    groupActor ! RequestTrackDevice("group", "device1", probe.ref)
    val registered2 = probe.receiveMessage()

    registered1.device should ===(registered2.device)
  }

  // Test that we get back the list of proper IDs once we have added a few devices
  "be able to list active devices" in {
    val registeredProbe = createTestProbe[DeviceRegistered]()
    val groupActor = spawn(DeviceGroup("group"))

    groupActor ! RequestTrackDevice("group", "device1", registeredProbe.ref)
    registeredProbe.receiveMessage()

    groupActor ! RequestTrackDevice("group", "device2", registeredProbe.ref)
    registeredProbe.receiveMessage()

    val deviceListProbe = createTestProbe[ReplyDeviceList]()
    groupActor ! RequestDeviceList(requestId = 0, groupId = "group", deviceListProbe.ref)
    deviceListProbe.expectMessage(ReplyDeviceList(requestId = 0, Set("device1", "device2")))
  }

  // Test case makes sure that the device ID is properly removed after the device actor has been stopped
  // TestProbe has a expectTerminated method that we can easily use to assert that the device actor has been terminated
  "be able to list active devices after one shuts down" in {
    val registeredProbe = createTestProbe[DeviceRegistered]()
    val groupActor = spawn(DeviceGroup("group"))

    groupActor ! RequestTrackDevice("group", "device1", registeredProbe.ref)
    val registered1 = registeredProbe.receiveMessage()
    val toShutDown = registered1.device

    groupActor ! RequestTrackDevice("group", "device2", registeredProbe.ref)
    registeredProbe.receiveMessage()

    val deviceListProbe = createTestProbe[ReplyDeviceList]()
    groupActor ! RequestDeviceList(requestId = 0, groupId = "group", deviceListProbe.ref)
    deviceListProbe.expectMessage(ReplyDeviceList(requestId = 0, Set("device1", "device2")))

    toShutDown ! Passivate
    registeredProbe.expectTerminated(toShutDown, registeredProbe.remainingOrDefault)

    // using awaitAssert to retry because it might take longer for the groupActor
    // to see the Terminated, that order is undefined
    registeredProbe.awaitAssert {
      groupActor ! RequestDeviceList(requestId = 1, groupId = "group", deviceListProbe.ref)
      deviceListProbe.expectMessage(ReplyDeviceList(requestId = 1, Set("device2")))
    }
  }
}

// 90 dollar an hour
// wrting akka with scala
// the baller that never call ya never halla
