#include <string>

#include <boost/python.hpp>
#include <boost/shared_ptr.hpp>

#include <addutil/network/IP_Connection.h>
#include <freekick/match/client/Network.h>
#include <freekick/match/MatchStatus.h>
#include <freekick/soccer/MatchData.h>

using namespace std;

using namespace boost::python;

using namespace addutil;
using namespace addutil::network;
using namespace freekick::soccer;
using namespace freekick::match;
using namespace freekick::match::client;

struct ClientWrap : Client, wrapper<Client>
{
    ClientWrap(IP_Connection conn)
        : Client(conn)
    {
    }

    void read(std::string buf)
    {
        this->get_override("read")(buf);
    }
};

// http://mail.python.org/pipermail/python-list/2002-July/156172.html
struct NetworkWrap : Network, wrapper<Network>
{
    NetworkWrap(IP_Connection conn, bool ai)
        : Network(conn, ai)
    {
    }

    boost::shared_ptr<MatchStatus> getMatchStatusPtr()
    {
        return boost::shared_ptr<MatchStatus>(getMatchStatus());
    }

    void read(std::string buf)
    {
        Network::read(buf);
    }
};

struct EntityWrap : Entity, wrapper<Entity>
{
    EntityWrap(float _mass = 0.0f, std::string _model = "")
        : Entity(_mass, _model)
    {
    }

    EntityWrap(const Entity& e)
        : Entity(e)
    {
    }

    Vector3 getPosition_() const
    {
        const Vector3& v = Entity::getPosition();
        return Vector3(v);
    }

    const int getID() const
    {
        return this->get_override("getID")();
    }
};

struct DynamicEntityWrap : DynamicEntity, wrapper<DynamicEntity>
{
    DynamicEntityWrap(float _mass = 0.0f, std::string _model = "")
        : DynamicEntity(_mass, _model)
    {
    }

    DynamicEntityWrap(const DynamicEntity& d)
        : DynamicEntity(d)
    {
    }

    const int getID() const
    {
        return this->get_override("getID")();
    }
};

struct MessageWrap : freekick::match::messages::Message, wrapper<freekick::match::messages::Message>
{
    MessageWrap() { }
    MessageWrap(const freekick::match::messages::Message& m) : freekick::match::messages::Message(m) { }
    const std::string toString() const
    {
        return this->get_override("toString")();
    }
};

struct StandardMessageWrap : freekick::match::messages::StandardMessage, wrapper<freekick::match::messages::StandardMessage>
{
    StandardMessageWrap() { }
    StandardMessageWrap(const freekick::match::messages::StandardMessage& m) : freekick::match::messages::StandardMessage(m) { }
    const std::string toString() const
    {
        return this->get_override("toString")();
    }
};

struct SingularMessageWrap : freekick::match::messages::SingularMessage, wrapper<freekick::match::messages::SingularMessage>
{
    SingularMessageWrap() { }
    SingularMessageWrap(const freekick::match::messages::SingularMessage& m) : freekick::match::messages::SingularMessage(m) { }
    const std::string toString() const
    {
        return this->get_override("toString")();
    }
};

BOOST_PYTHON_MODULE(libfreekick_client_py)
{
    class_<IP_Connection>("IP_Connection")
        .def_readwrite("ip_address", &IP_Connection::ip_address)
        .def_readwrite("port", &IP_Connection::port)
        ;

    class_<MessageWrap>("Message")
        .def("toString", pure_virtual(&MessageWrap::toString))
        ;

    class_<StandardMessageWrap, bases<freekick::match::messages::Message> >("StandardMessage");

    class_<SingularMessageWrap, bases<freekick::match::messages::StandardMessage> >("SingularMessage");

    class_<freekick::match::messages::InitialDataRequest, bases<freekick::match::messages::SingularMessage> >("InitialDataRequest");

    class_<ClientWrap, boost::noncopyable>("Client", init<IP_Connection>())
        .def("is_connected", &ClientWrap::is_connected)
//        .def("read", &ClientWrap::read)
        .def("read", pure_virtual(&ClientWrap::read))
        .def("read_connection", &ClientWrap::read_connection)
        ;

    class_<Stadium>("Stadium", init<optional<float, float> >())
        ;

    class_<Club>("Club", init<const std::string&>())
        .def("getNumberOfPlayers", &Club::getNumberOfPlayers)
        ;

    class_<MatchData>("MatchData", init<boost::shared_ptr<Club>, boost::shared_ptr<Club>, boost::shared_ptr<Stadium> >())
        .def("getHomeClubName", &MatchData::getHomeClubName)
        ;

    class_<Vector3>("Vector3", init<float, float, float>())
        .def_readwrite("x", &Vector3::x)
        .def_readwrite("y", &Vector3::y)
        .def_readwrite("z", &Vector3::z)
        ;

    class_<Human>("Human", no_init)
        ;

    class_<EntityWrap>("Entity", init<optional<float, string> >())
        .def("getPosition", &EntityWrap::getPosition_)
        .def("getID", &EntityWrap::getID)
        ;

    class_<DynamicEntityWrap, bases<EntityWrap> >("DynamicEntity", init<optional<float, string> >())
        ;

    enum_<PlayerPosition>("PlayerPosition")
        .value("Goalkeeper", Goalkeeper)
        .value("Defender", Defender)
        .value("Midfielder", Midfielder)
        .value("Forward", Forward)
        ;

    class_<Player, bases<Human> >("Player", init<const string&, int, unsigned int, PlayerPosition>());

    class_<MatchPlayer, bases<Player, DynamicEntity> >("MatchPlayer", init<const Player&, bool>())
        .def("getID", &MatchPlayer::getID)
        .def("isSubstitute", &MatchPlayer::isSubstitute)
        ;

    class_<MatchStatus>("MatchStatus", init<boost::shared_ptr<MatchData> >())
        .def("getPitchWidth", &MatchStatus::getPitchWidth)
        .def("getPlayers", &MatchStatus::getPlayers)
        ;

    class_<NetworkWrap, bases<Client>, boost::noncopyable>("Network", init<IP_Connection, bool>())
        .def("run", &NetworkWrap::run)
        .def("getMatchStatus", &NetworkWrap::getMatchStatus, return_value_policy<manage_new_object>())
        .def("read", &NetworkWrap::read)
        .def("sendMessage", &NetworkWrap::sendMessage)
        ;

}
