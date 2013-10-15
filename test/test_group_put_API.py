#!/usr/bin/python
import requests
import unittest

# Required to run this file
# - Python 3
# - 'Requests' library installed on local machine

class TestStreamAPI(unittest.TestCase):

    def setUp(self):
        self.base_url = "http://localhost:8000"
        self.group_url = "/groups/"              #edit with existing type name
        self.user_url = "/users/"                #edit with existing type name
        self.user = "lDe1G5IiQCGpBu4B2aeBfw"     #edit with existing user id
        self.group = "2TtaGDOLSES7iVt4nx4p0Q"    #edit with existing group id
        self.stream = '"TEST"' #edit with existing stream id
        self.json_headers ={"Content-Type" : "application/json"}
        self.update_group = '{"input":['+self.stream+']}'

# PUT TEST
    #add stream into existing group - full url
    def test_put_stream_full_url(self):
        url = self.base_url + self.user_url + self.user + self.group_url + self.group
        resp = requests.put(url, data=self.update_group, headers=self.json_headers)
        self.assertEqual(resp.status_code, 204)

    #add stream into existing group - partial url
    def test_put_stream_partial_url(self):
        url = self.base_url + self.group_url + self.group
        resp = requests.put(url, data=self.update_group, headers=self.json_headers)
        self.assertEqual(resp.status_code, 204)

    #add stream into non-existing group - full url
    def test_put_non_group_full_url(self):
        url = self.base_url + self.user_url + self.user + self.group_url + 'dummy_group_id'
        resp = requests.put(url, data=self.update_group, headers=self.json_headers)
        self.assertEqual(resp.status_code, 500)

        #add stream into non-existing group - partial url
    def test_put_non_group_partial_url(self):
        url = self.base_url + self.group_url + 'dummy_group_id'
        resp = requests.put(url, data=self.update_group, headers=self.json_headers)
        self.assertEqual(resp.status_code, 500)

        #add stream into existing group with wrong user id - full url
    def test_put_stream_non_user_full_url(self):
        url = self.base_url + self.user_url + "dummy_id" + self.group_url + self.group
        resp = requests.put(url, data=self.update_group, headers=self.json_headers)
        self.assertEqual(resp.status_code, 500)


if __name__ == "__main__":
    suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestStreamAPI)
    unittest.TextTestRunner.run(suite)

