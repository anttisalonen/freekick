#include <iostream>

#include "Vector3.h"

#include "ClientInitMessage.h"
#include "ServerInitMessage.h"
#include "GeneralIntervalAck.h"
#include "GeneralError.h"
#include "GeneralUpdatePauseMessage.h"
#include "MovePlayerControlMessage.h"
#include "SetGeneralUpdateIntervalMessage.h"
#include "UsernameRequestMessage.h"
#include "ListOfPlayersMessage.h"

using namespace freekick::match::messages;
using addutil::Vector3;

int main()
{
    std::set<PlayerID> pls1;
    pls1.insert(29756);
    pls1.insert(5058);
    ClientInitMessage cim("client name", "0.2", "user", "H", pls1);
    ServerInitMessage sim("server name", "0.2", "welcome!");
    std::cout << "Client:\n" << cim.toString();
    std::cout << "Server:\n" << sim.toString();

    GeneralIntervalAck gia;
    std::cout << "General Interval Ack:\n" << gia.toString() << std::endl;

    GeneralError ge;
    std::cout << "General Error Ack:\n" << ge.toString() << std::endl;

    GeneralUpdatePauseMessage gupm(false);
    std::cout << "General Update Pause:\n" << gupm.toString() << std::endl;

    MovePlayerControlMessage mpcm(1234, Vector3(3.27f, 0.0f, -48.5f));
    std::cout << "Move Player Control:\n" << mpcm.toString() << std::endl;

    SetGeneralUpdateIntervalMessage sguim(1000);
    std::cout << "Set General Update Interval:\n" << sguim.toString() << std::endl;

    UsernameRequestMessage urm("Username");
    std::cout << "Username Request:\n" << urm.toString() << std::endl;

    std::set<PlayerID> pls2;
    pls2.insert(2386);
    pls2.insert(363);
    pls2.insert(856);
    ListOfPlayersMessage lopm(pls2);
    std::cout << "List of Players:\n" << lopm.toString() << std::endl;

    return 0;
}
