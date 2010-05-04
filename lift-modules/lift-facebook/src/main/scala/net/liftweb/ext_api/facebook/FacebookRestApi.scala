/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package ext_api {
package facebook {

import _root_.java.net.{HttpURLConnection, URL, URLEncoder}
import _root_.java.io.DataOutputStream
import _root_.java.io.InputStream
import _root_.java.util.Date

import _root_.scala.xml.{Node, XML, NodeSeq}

import _root_.net.liftweb.util.Helpers._

object FacebookRestApi {
  def apiKey = System.getProperty("com.facebook.api_key")
  def secret = System.getProperty("com.facebook.secret")
  def apiKey_=(key: String) = System.setProperty("com.facebook.api_key", key)
  def secret_=(key: String) = System.setProperty("com.facebook.secret", key)
}

object FacebookClient {
  import FacebookRestApi._
  
  var readTimeout = 10.seconds
  var connectTimeout = 10.seconds
  
  val TARGET_API_VERSION = "1.0"
  val FB_SERVER = "api.facebook.com/restserver.php"
  val SERVER_ADDR = "http://" + FB_SERVER
  val HTTPS_SERVER_ADDR = "https://" + FB_SERVER

  val SERVER_URL = new URL(SERVER_ADDR)
  val HTTPS_SERVER_URL = new URL(HTTPS_SERVER_ADDR)

  val CrLf = "\r\n"
  val Pref = "--"

  def urlEncode(name: String): String = URLEncoder.encode(name, "UTF-8")

  def stripSig(in: String): String =  if (in != null && in.startsWith("fb_sig_")) in.substring(7) else in

  def convert(in: List[(String, Any)]): List[String] = in.map{case (name, value) => stripSig(name)+"="+value}

  def byteToHex(b: Byte): String = Integer.toHexString((b & 0xf0) >>> 4) + Integer.toHexString(b & 0x0f)

  def genSignature(allParams: List[(String, Any)], secret: String): String = {
    val md = _root_.java.security.MessageDigest.getInstance("MD5")
    val theStr = convert(allParams).sort(_ < _).mkString("") + secret

    md.digest((theStr).getBytes).map(byteToHex(_)).mkString("")
  }

  private[facebook] def call[T](params: List[(String, Any)], parser:InputStream => T): T = {
    val theParams = params.map{case (name, value) => urlEncode(name)+"="+urlEncode(value.toString)}.mkString("&")

    SERVER_URL.openConnection match {
      case conn: HttpURLConnection => {
        conn.setReadTimeout(readTimeout.millis.toInt)
        conn.setConnectTimeout(connectTimeout.millis.toInt)
        
        conn.setRequestMethod("POST") // [ticket #27]
        conn.setDoOutput(true)
        conn.connect
        conn.getOutputStream.write(theParams.getBytes())
        
        parser(conn.getInputStream())
      }
    }
  }
  private[facebook] def call(params: List[(String, Any)]): Node = call(params, xmlParser)
  
  def xmlParser(is:InputStream):Node = XML.load(is)
  
  private[facebook] def buildParams(methodName: String, params: FacebookParam*): List[(String, Any)] = {
    val allParams: List[(String, Any)] =
      ("method", methodName) ::
      ("api_key", apiKey) ::
      ("v",  TARGET_API_VERSION) ::
      params.map(p => (p.key, p.value)).toList

    val signature = genSignature(allParams, secret)

    val ret = "sig" -> signature :: allParams
    ret
  }

  def callMethod(meth: SessionlessFacebookMethod): Node =
    call(buildParams(meth.name, meth.params: _*))

  def !?(meth: SessionlessFacebookMethod): Node =
    callMethod(meth)

  def fromSession(session: FacebookSession) : FacebookClient[Node] = {
    new FacebookClient(session, xmlParser,FacebookFormat.xml)
  }

  def fromAuthToken(authToken: String) : Option[FacebookClient[Node]] = {
    FacebookSession.fromAuthToken(authToken).map(fromSession)
  }

  type State = {
    def sessionKey: Option[String]
    def expiration: Option[Long]
    def uid: Option[String]
  }

  def fromState(implicit state: State) : Option[FacebookClient[Node]] = {
    for (
      key <- state.sessionKey;
      exp <- state.expiration;
      uid <- state.uid
    ) yield fromSession(FacebookSession(key, exp, uid))
  }
}

object FacebookFormat extends Enumeration("XML", "JSON"){
  val xml, json = Value
}

class FacebookClient[T](val apiKey: String, val secret: String, val session: FacebookSession, parser: InputStream=>T, format: FacebookFormat.Value) {
  import FacebookRestApi._
  import FacebookClient._

  def this(session: FacebookSession, parser:InputStream => T, format:FacebookFormat.Value) = this(FacebookRestApi.apiKey, FacebookRestApi.secret, session, parser, format)

  def callMethod(meth: FacebookMethod, fileName: String, mimeType: String, file: Array[Byte], params: FacebookParam* ): T = {
    val boundary = System.currentTimeMillis.toString
    SERVER_URL.openConnection match {
      case conn: HttpURLConnection => {
        conn.setReadTimeout(readTimeout.millis.toInt)
        conn.setConnectTimeout(connectTimeout.millis.toInt)
        
        conn.setDoInput(true)
        conn.setDoOutput(true)
        conn.setUseCaches(false)
        conn.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary)
        conn.setRequestProperty("MIME-version", "1.0")

        val out = new DataOutputStream(conn.getOutputStream())

        buildParams(meth, params: _*).foreach {
          case (name, value) =>
            out.writeBytes(Pref + boundary + CrLf)
            out.writeBytes("Content-disposition: form-data; name='" + name + "'")
            out.writeBytes(CrLf + CrLf + value.toString + CrLf)
        }

        out.writeBytes(Pref + boundary + CrLf)
        out.writeBytes("Content-disposition: form-data; filename='" +
                       fileName + "'" + CrLf)
        out.writeBytes("Content-Type: " + mimeType + CrLf + CrLf)

        out.write(file)

        out.writeBytes(CrLf + Pref + boundary + Pref + CrLf)

        out.flush()
        out.close()
        
        parser(conn.getInputStream)
      }
    }
  }

  def callMethod(meth: FacebookMethod): T =
    call(buildParams(meth, meth.params: _*), parser)

  def callMethod(meth: FacebookMethod, otherParams: FacebookParam*): T =
    call(buildParams(meth, (meth.params.toList ::: otherParams.toList): _*), parser)

  def !?(meth: FacebookMethod): T = callMethod(meth)

  def !?(meth: FacebookMethod, otherParams: UniversalParam*) = callMethod(meth, otherParams: _*)

  def !?(meth: UploadPhoto): T = callMethod(meth, meth.fileName, meth.mimeType, meth.fileData)

  private def buildParams(meth: FacebookMethod, params: FacebookParam*): List[(String, Any)] = {
    val allParams: List[FacebookParam] = FacebookParam("format" -> format.toString)::
      (if (meth.requiresSession)
        List(FacebookParam("call_id" -> System.currentTimeMillis), FacebookParam("session_key" -> session.key))
      else
        Nil) :::
      params.toList

    FacebookClient.buildParams(meth.name, allParams: _*)
  }

  def getInfo(users: Collection[Long], fields: FacebookField*): T = {
    callMethod(GetInfo(users, fields: _*))
  }
}

object FacebookSession {
  def apply(key: String, expiration: Long, uid: String) : FacebookSession =
    new FacebookSession(key, expiration, uid)

  def fromAuthToken(authToken: String): Option[FacebookSession] = {
    val response = FacebookClient !? AuthGetSession(authToken)
    val key = (response \\ "session_key").text
    val uid = (response \\ "uid").text
    val expiration = (response \\ "expires").text

    if (key == "")
      None
    else
      Some(FacebookSession(key, expiration.toLong, uid))
  }
}

class FacebookSession(val key: String, val expiration: Long, val uid: String)

class FacebookMethod(val name: String, attachment: Boolean, val params: FacebookParam*) {
  def this(nm: String, params: FacebookParam*) = { this(nm, false, params: _*) }

  def requiresSession: Boolean = true
}

class SessionlessFacebookMethod(override val name: String, override val params: FacebookParam*) extends FacebookMethod(name, false, params: _*) {

  override def requiresSession = false
}


case object AuthCreateToken extends SessionlessFacebookMethod("facebook.auth.createToken")
case class AuthGetSession(authToken: String) extends SessionlessFacebookMethod("facebook.auth.getSession", AuthToken(authToken))
case class GetFriends(optionalParams: GetFriendsParam*) extends FacebookMethod("facebook.friends.get", optionalParams: _*)
case object GetFriendLists extends FacebookMethod("facebook.friends.getLists")
case class FqlQuery(query: String) extends FacebookMethod("facebook.fql.query", Query(query))
case class FqlMultiQuery(queries: Map[String,String]) extends FacebookMethod("facebook.fql.multiquery", FacebookParam("queries", {
    queries.map(p => "\""+p._1+"\":\""+p._2+"\"").mkString("{",",","}")
}))
case class GetEvents(filters: GetEventsParam*) extends FacebookMethod("facebook.events.get", filters: _*)
case class GetEventsMembers(eventId: Long) extends FacebookMethod("facebook.events.getMembers", EventId(eventId))
case object GetAppUsers extends FacebookMethod("facebook.friends.getAppUsers")
//case object GetRequests extends FacebookMethod("facebook.friends.getRequests") /*This method is not listed in the current facebook api. deprecated?*/
case class AreFriends(friends1: Collection[Long], friends2: Collection[Long]) extends FacebookMethod("facebook.friends.areFriends", FacebookParam("uids1", friends1.mkString(",")), FacebookParam("uids2", friends2.mkString(",")))
case class GetInfo(users: Collection[Long], fields: FacebookField*) extends FacebookMethod("facebook.users.getInfo", UserIds(users.toList: _*), FacebookFields(fields: _*))
case object GetUser extends FacebookMethod("facebook.users.getLoggedInUser")
case class GetPhotos(primaryFilter: GetPhotosParam, otherFilters: GetPhotosParam*) extends FacebookMethod("facebook.photos.get", (primaryFilter :: otherFilters.toList): _*)
case class GetAlbums(primaryFilter: GetAlbumsParam, otherFilters: GetAlbumsParam*) extends FacebookMethod("facebook.photos.getAlbums", (primaryFilter :: otherFilters.toList): _*)
case class GetPhotoTags(photoIds: Long*) extends FacebookMethod(" facebook.photos.getTags", PhotoIds(photoIds: _*))
case class CreatePhotoAlbum(albumName: String, otherParams: CreatePhotoAlbumParam*) extends FacebookMethod("facebook.photos.createAlbum", (NameParam(albumName) :: otherParams.toList): _*)
case class AddPhotoTags(photoId: Long, tags: PhotoTag*) extends FacebookMethod("facebook.photos.addTag", PhotoId(photoId), Tags(tags: _*))
case class UploadPhoto(fileName: String, mimeType: String, fileData: Array[Byte], otherParams: UploadPhotoParam*) extends FacebookMethod("facebook.photos.upload", true, otherParams: _*)
case object GetNotifications extends FacebookMethod("facebook.notifications.get")
case class SendNotifications(notification: NodeSeq, recipients: Long*) extends FacebookMethod("facebook.notifications.send", RecipientIds(recipients: _*), Notification(notification))
//case class SendRequest extends FacebookMethod("facebook.notifications.sendRequest", 5) /*This method was disabled by the facebook api*/
case class GetGroups(filters: GetGroupsParam*) extends FacebookMethod("facebook.groups.get", filters: _*)
case class GetGroupMembers(groupId: Long) extends FacebookMethod("facebook.groups.getMembers", GroupId(groupId))
case class SetFBML(optionalParams: SetFbmlParam*) extends FacebookMethod("facebook.profile.setFBML", optionalParams: _*)
case class GetFBML(optionalParams: GetFbmlParam*) extends FacebookMethod("facebook.profile.getFBML", optionalParams: _*)
case class RefreshImage(imageUrl: String) extends FacebookMethod("facebook.fbml.refreshImgSrc", Url(imageUrl))
case class RefreshRefURL(refUrl: String) extends FacebookMethod("facebook.fbml.refreshRefUrl", Url(refUrl))
case class SetRefHandle(handle: String, markup: NodeSeq) extends FacebookMethod("facebook.fbml.setRefHandle", RefHandle(handle), FBML(markup))

case class StreamPublish(publishParams: StreamPublishParam*) extends FacebookMethod("facebook.stream.publish", (publishParams.toList): _*)

class FacebookField(val name: String)

case object AboutMe extends FacebookField("about_me")
case object Activities extends FacebookField("activities")
case object Affiliations extends FacebookField("affiliations")
case object Birthday extends FacebookField("birthday")
case object Books extends FacebookField("books")
case object CurrentLocation extends FacebookField("current_location")
case object EducationHistory extends FacebookField("education_history")
case object FirstName extends FacebookField("first_name")
case object AddedApp extends FacebookField("has_added_app")
case object Hometown extends FacebookField("hometown_location")
case object Highschool extends FacebookField("hs_info")
case object Interests extends FacebookField("interests")
case object AppUser extends FacebookField("is_app_user")
case object LastName extends FacebookField("last_name")
case object MeetingFor extends FacebookField("meeting_for")
case object LookingFor extends FacebookField("meeting_for")
case object MeetingSex extends FacebookField("meeting_sex")
case object InterestedIn extends FacebookField("meeting_sex")
case object Movies extends FacebookField("movies")
case object Music extends FacebookField("music")
case object Name extends FacebookField("name")
case object NotesCount extends FacebookField("notes_count")
case object Pic extends FacebookField("pic")
case object BigPic extends FacebookField("pic_big")
case object SmallPic extends FacebookField("pic_small")
case object SquarePic extends FacebookField("pic_square")
case object PoliticalView extends FacebookField("political")
case object UpdateTime extends FacebookField("profile_update_time")
case object Quotes extends FacebookField("quotes")
case object Relationship extends FacebookField("relationship_status")
case object RelationshipStatus extends FacebookField("relationship_status")
case object Religion extends FacebookField("religion")
case object Sex extends FacebookField("sex")
case object SignificantOther extends FacebookField("significant_other_id")
case object Status extends FacebookField("status")
case object Timezone extends FacebookField("timezone")
case object TV extends FacebookField("tv")
case object WallCount extends FacebookField("wall_count")
case object WorkHistory extends FacebookField("work_history")
case object Email extends FacebookField("email")
case object Username extends FacebookField("username")

sealed abstract class PhotoTag(x: Double, y: Double){ def toJSON: String }
case class TagById(userId: Long, x: Double, y: Double) extends PhotoTag(x, y){
  def toJSON = "{'x':'" + x.toString + "','y':'" + y.toString + "','tag_uid':" + userId.toString + "}"
}
case class TagByName(name: String, x: Double, y: Double) extends PhotoTag(x, y){
  def toJSON = "{'x':'" + x.toString + "','y':'" + y.toString + "','tag_text':'" + name.toString + "'}"
}

object FacebookParam {
  def apply(key: String, value: Any) = new FacebookParam(key, value)
  def apply(pair: (String, Any)) = new FacebookParam(pair)
}

class FacebookParam(val key: String, val value: Any){
  def this(pair: (String, Any)) = this(pair._1, pair._2)
}
trait UniversalParam extends FacebookParam
trait GetFriendsParam extends FacebookParam
trait GetEventsParam extends FacebookParam
trait GetPhotosParam extends FacebookParam
trait GetAlbumsParam extends FacebookParam
trait GetPhotoTagsParam extends FacebookParam
trait CreatePhotoAlbumParam extends FacebookParam
trait AddPhotoTagsParam extends FacebookParam
trait SendNotificationParam extends FacebookParam
trait GetGroupsParam extends FacebookParam
trait GetGroupMembersParam extends FacebookParam
trait SetFbmlParam extends FacebookParam
trait GetFbmlParam extends FacebookParam
trait RefreshImageParam extends FacebookParam
trait RefreshRefParam extends FacebookParam
trait SetRefHandleParam extends FacebookParam
trait StreamPublishParam extends FacebookParam
trait UploadPhotoParam extends FacebookParam

case class Callback(functionName: String) extends FacebookParam("callback", functionName) with UniversalParam

case class AuthToken(token: String) extends FacebookParam("auth_token", token)
case class Query(query: String) extends FacebookParam("query", query)
case class FacebookFields(fields: FacebookField*) extends FacebookParam("fields", fields.map(_.name).mkString(","))
case class FriendListId(friendListId: Long) extends FacebookParam("flid", friendListId) with GetFriendsParam
case class UserId(userId: Long) extends FacebookParam("uid", userId) with GetEventsParam with GetAlbumsParam with GetGroupsParam with SetFbmlParam with GetFbmlParam
case class UserIds(userIds: Long*) extends FacebookParam("uids", userIds.mkString(","))
case class EventId(eventId: Long) extends FacebookParam("eid", eventId)
case class EventIds(eventIds: Long*) extends FacebookParam("eids", eventIds.mkString(",")) with GetEventsParam
case class StartTime(startTime: Date) extends FacebookParam("start_time", startTime.getTime()) with GetEventsParam
case class EndTime(endTime: Date) extends FacebookParam("end_time", endTime.getTime()) with GetEventsParam
case object RsvpAttending extends FacebookParam("rsvp_status", "attending") with GetEventsParam
case object RsvpUnsure extends FacebookParam("rsvp_status", "unsure") with GetEventsParam
case object RsvpDeclined extends FacebookParam("rsvp_status", "declined") with GetEventsParam
case object RsvpNotReplied extends FacebookParam("rsvp_status", "not_replied") with GetEventsParam
case class SubjectId(subjectId: Long) extends FacebookParam("subj_id", subjectId) with GetPhotosParam
case class AlbumId(albumId: Long) extends FacebookParam("aid", albumId) with GetPhotosParam with AddPhotoTagsParam with UploadPhotoParam
case class AlbumIds(albumIds: Long*) extends FacebookParam("aids", albumIds.mkString(",")) with GetAlbumsParam
case class PhotoId(photoId: Long) extends FacebookParam("pid", photoId) with AddPhotoTagsParam
case class PhotoIds(photoIds: Long*) extends FacebookParam("pids", photoIds.mkString(",")) with GetPhotosParam with GetPhotoTagsParam
case class NameParam(name: String) extends FacebookParam("name", name) with CreatePhotoAlbumParam
case class Location(location: String) extends FacebookParam("location", location) with CreatePhotoAlbumParam
case class Description(description: String) extends FacebookParam("description", description) with CreatePhotoAlbumParam
case class TagUserId(userId: Long) extends FacebookParam("tag_uid", userId)
case class TagText(text: String) extends FacebookParam("tag_text", text)
case class Tags(tags: PhotoTag*) extends FacebookParam("tags", tags.map(_.toJSON).mkString("[", ",", "]"))
case class Notification(markup: NodeSeq) extends FacebookParam("notification", markup) with SendNotificationParam
case class RecipientIds(recipientIds: Long*) extends FacebookParam("to_ids", recipientIds.mkString(",")) with SendNotificationParam
case class GroupId(groupId: Long) extends FacebookParam("gid", groupId) with GetGroupMembersParam
case class GroupIds(groupIds: Long*) extends FacebookParam("gids", groupIds.mkString(",")) with GetGroupsParam
case class ProfileMarkup(markup: NodeSeq) extends FacebookParam("profile", markup) with SetFbmlParam
case class ProfileActionMarkup(markup: NodeSeq) extends FacebookParam("profile_action", markup) with SetFbmlParam
case class MobileProfileMarkup(markup: NodeSeq) extends FacebookParam("mobile_profile", markup) with SetFbmlParam
case class Url(url: String) extends FacebookParam("url", url) with RefreshImageParam with RefreshRefParam
case class RefHandle(handle: String) extends FacebookParam("handle", handle) with SetRefHandleParam
case class FBML(markup: NodeSeq) extends FacebookParam("fbml", markup) with SetRefHandleParam
case class Caption(caption: String) extends FacebookParam("caption", caption) with UploadPhotoParam

case class TargetId(id:String) extends FacebookParam("target_id", id) with StreamPublishParam
case class StreamMessage(data: String) extends FacebookParam("message", data) with StreamPublishParam
case class StreamAttachment(data: String) extends FacebookParam("attachment", data) with StreamPublishParam
case class StreamLinks(data: String) extends FacebookParam("action_links", data) with StreamPublishParam

}
}
}
