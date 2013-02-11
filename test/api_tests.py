# -*- coding: utf-8 -*-
 
#!/usr/local/Cellar/python/2.7.3/bin/python
import requests
import unittest
import json
 
class TestAPI(unittest.TestCase):
 
  def setUp(self):
    self.base_url = "http://localhost:8001"
    self.json_headers ={"Content-Type" : "application/json", "Accept" : "application/json"}
    self.form_headers ={"Content-Type" : "application/x-www-form-urlencoded", "Accept" : "application/json"}
 
  def test_get_on_root_returns_ok(self):
    resp = requests.get(self.base_url)
    self.assertEqual(resp.content, "OK")
 
  def test_new_resource_and_get_it(self):
    url = self.base_url + "/api/product"
    resp = requests.post(url, data={"name":"TV 62 LCD"}, headers=self.form_headers)
    self.assertEqual(resp.status_code, 201)

    key = json.loads(resp.content)['key']
    resp2 = requests.get(url + "/" + key)
    self.assertEqual(resp2.status_code, 200)

  
  def test_update_existing_resource_with_put(self):
    # create
    url = self.base_url + "/api/product"
    resp = requests.post(url, data={"name":"TV 62 LCD"}, headers=self.form_headers)
    self.assertEqual(resp.status_code, 201)
    key = json.loads(resp.content)['key']
    
    # update
    url = self.base_url + "/api/product/" + key
    resp = requests.put(url, data={"name":"TV 50 LCD"}, headers=self.form_headers)
    self.assertEqual(resp.status_code, 200)

    resp2 = requests.get(url)
    self.assertEqual(resp2.status_code, 200)
    self.assertEqual(resp2.content, '{"key":"'+key+'","name":"TV 50 LCD"}')

  def test_delete_resource(self):
    url = self.base_url + "/api/product"
    resp = requests.post(url, data={"name":"TV 62 LCD"}, headers=self.form_headers)
    self.assertEqual(resp.status_code, 201)
    key = json.loads(resp.content)['key']

    resp2 = requests.delete(url + "/" + key)
    self.assertEqual(resp2.status_code, 200)
 
if __name__ == "__main__":
  suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestAPI)
  unittest.TextTestRunner.run(suite)