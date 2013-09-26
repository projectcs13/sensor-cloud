#!/usr/bin/python
import requests
import unittest
 
class TestStreamAPI(unittest.TestCase):

    def setUp(self):
        self.base_url = "http://localhost:8000"
	self.stream_url = self.base_url+"/streams/"
        self.json_headers ={"Content-Type" : "application/json", "Accept" : "application/json"}
	self.new_stream = {"description": "Ile"}
	self.new_stream2 = {"description": "Vilaine"}
	self.new_stream3 = {"description": "Seine"}

# Test if the server is up

    def test_get_on_root_returns_html_hello_world(self):
        resp = requests.get(self.base_url)
        self.assertEqual(resp.content, '<html><body>Hello, new world</body></html>')

# PUT

    def test_put_new_stream(self):
        url = self.stream_url + '1'

        resp = requests.put(url, data=self.new_stream, headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)
        self.assertEqual(resp.content, '{"id":"1","description":"Ile"}')

	# Test durability

	resp2 =requests.get(url, headers=self.json_headers)
	self.assertEqual(resp.content, '{"id":"1","description":"Ile"}')

    def test_put_updates_stream(self):
        url = self.stream_url + '1'

        resp = requests.get(url)
        self.assertEqual(resp.status_code, 200)
 
        resp2 = requests.put(url, data=self.new_stream2, headers=self.json_headers)
        self.assertEqual(resp2.status_code, 200)
        self.assertEqual(resp2.content, '{"id":"1","description":"Vilaine"}')
 
    def test_put_without_data_is_malformed(self):
        url = self.stream_url + '2'
        resp = requests.put(url, data = {}, headers=self.json_headers)
        self.assertEqual(resp.status_code, 400)

# POST

    def test_post_new_stream(self):
        resp = requests.post(self.stream_url, data=self.new_stream3,
            headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)
        self.assertNotEqual(resp.content, '')

# GET 

    def test_get_on_stream_returns_id_in_json(self):
        resp = requests.get(self.stream_url + '0', headers=self.json_headers)
        self.assertEqual(resp.status_code, 200)
        self.assertEqual(resp.content, '{"id":"0","description":"Rhone"}')

    def test_get_on_nonexisting_stream_returns_404(self):
        resp = requests.get(self.stream_url + '123')
        self.assertEqual(resp.status_code, 404)

# DELETE

    def test_delete_stream(self):
	url = self.stream_url + '1'

        resp1 = requests.delete(url)
        self.assertEqual(resp1.status_code, 204)
 
        # Test durability
        resp2 = requests.get(url)
        self.assertEqual(resp2.status_code, 404)

if __name__ == "__main__":
    suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestStreamAPI)
    unittest.TextTestRunner.run(suite)
