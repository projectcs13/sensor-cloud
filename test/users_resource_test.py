#!/usr/bin/python
import requests
import unittest
 
class TestUser_Resource(unittest.TestCase):

	def setUp(self):
		self.base_url = "http://localhost:8000"
		self.user_url = self.base_url+"/users/"
		self.json_headers ={"Content-Type" : "application/json"}
		self.new_user = "{\"description\": \"Ile\"}"
		self.new_user2 = "{\"description\": \"Vilaine\"}"
		self.new_user3 = "{\"description\": \"Seine\"}"
		self.new_user4 = "{\"description\": \"Trol\"}"


	# DELETE

	def test_delete_user(self):
		url = self.user_url + '1'

		resp = requests.post(self.user_url, data=self.new_user4,
		    headers=self.json_headers)
		self.assertEqual(resp.status_code, 201)

		resp1 = requests.delete(url)
		self.assertEqual(resp1.status_code, 204)
	 
		# Test durability
		resp2 = requests.get(url)
		self.assertEqual(resp2.status_code, 404)

	def test_delete_user_when_not_exists(self):
		url = self.user_url + '0'
		resp1 = requests.delete(url)
		self.assertEqual(resp1.status_code, 404)


	# POST

	def test_post_new_user(self):
		resp = requests.post(self.user_url, data=self.new_user,
		    headers=self.json_headers)
		self.assertEqual(resp.status_code, 201)


	# PUT


	def test_put_updates_user(self):
		url = self.user_url + '3'

		resp = requests.get(url)
		self.assertEqual(resp.status_code, 200)
	 
		resp2 = requests.put(url, data=self.new_user2, headers=self.json_headers)
		self.assertEqual(resp2.status_code, 204)


		resp3 = requests.get(url)
		self.assertEqual(resp3.content, '{"description":"Vilaine"}')
	 
	# GET 

	def test_get_on_user_returns_id_in_json(self):
		resp = requests.post(self.user_url, data=self.new_user3,
		headers=self.json_headers)

		resp = requests.get(self.user_url + '2')
		self.assertEqual(resp.content, '{"description":"Seine"}')
		self.assertEqual(resp.status_code, 200)

	def test_get_on_nonexisting_user_returns_404(self):
		resp = requests.get(self.user_url + '0')
		self.assertEqual(resp.status_code, 404)



if __name__ == "__main__":
	unittest.main()
