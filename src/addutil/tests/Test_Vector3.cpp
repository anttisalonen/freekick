#include "addutil/Vector3.h"

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE Vector3 test
#include <boost/test/unit_test.hpp>


using namespace addutil;

BOOST_AUTO_TEST_CASE(operators)
{
    Vector3 v1(1.0f, 2.0f, 3.0f);
    Vector3 v2;
    v2 += v1;
    BOOST_CHECK_EQUAL(v2, Vector3(1.0f, 2.0f, 3.0f));
    Vector3 v3(1.0f, 0.0f, 0.0f);
    v3 *= 3.2f;
    Vector3 v4(0.0f, 0.0f, 0.0f);
    BOOST_CHECK_EQUAL(v3, Vector3(3.2f, 0.0f, 0.0f));
    v4 = v3 + v2;
    BOOST_CHECK_EQUAL(v4, Vector3(4.2f, 2.0f, 3.0f));
    Vector3 v5(3.0f, 4.0f, 0.0f);
    BOOST_CHECK_EQUAL(v5.normalized(), Vector3(0.6f, 0.8f, 0.0f));
    v5.normalize();
    BOOST_CHECK_EQUAL(v5, Vector3(0.6f, 0.8f, 0.0f));
    v2 -= v1;
    BOOST_CHECK_EQUAL(v2, Vector3(0.0f, 0.0f, 0.0f));
    Vector3 v6(5.0f, 8.0f, 2.0f);
    v2 = v2 - v6;
    BOOST_CHECK_EQUAL(v2, Vector3(-5.0f, -8.0f, -2.0f));
}

BOOST_AUTO_TEST_CASE(arithmetic)
{
    Vector3 v1(3.0f, 3.0f, 2.0f);
    Vector3 v2(2.0f, -6.0f, 0.0f);
    BOOST_CHECK_EQUAL(v1.dot(v2), -12.0f);
    BOOST_CHECK_EQUAL(v1.cross(v2), Vector3(12.0f, 4.0f, -24.0f));
}

BOOST_AUTO_TEST_CASE(angle)
{
    Vector3 v1(3.0f, 2.0f, 3.0f);
    Vector3 v2(5.0f, 7.0f, -5.0f);
    Vector3 v3(-2.0f, 1.0f, 0.0f);
    BOOST_CHECK(fabs(v1.angleBetweenXZ(v2)) - pi_2 < 0.01f);
    BOOST_CHECK(fabs(v2.angleBetweenXZ(v3)) - (pi_2 + pi_4) < 0.01f);
    BOOST_CHECK(fabs(v3.angleBetweenXZ(v1)) - (pi_2 + pi_4) < 0.01f);
}

