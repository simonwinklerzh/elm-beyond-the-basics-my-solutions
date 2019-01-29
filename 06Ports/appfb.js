'use strict';
// Initialize Firebase
var config = {
  apiKey: "AIzaSyAz8tt6onua0maMOt0eN5uNkqswNzAMtxA",
  authDomain: "customer-list-ef58e.firebaseapp.com",
  databaseURL: "https://customer-list-ef58e.firebaseio.com",
  projectId: "customer-list-ef58e",
  storageBucket: "customer-list-ef58e.appspot.com",
  messagingSenderId: "566249371650"
};

var app = firebase.initializeApp(config);
var database = app.database();
var CUSTOMERREFPATH = "customers"

function addCustomer(customer){
  var promise = database
    .ref(CUSTOMERREFPATH)
    .push(customer);
  return promise;
}

function updateCustomer(customer){
  var id = customer.id;
  var promise = database
    .ref(CUSTOMERREFPATH + "/" + id)
    .set(customer);
  return promise;
}

function deleteCustomer(customer){
  var id = customer.id;
  var promise = database
    .ref(CUSTOMERREFPATH + "/" + id)
    .remove();
  return promise;
}

function customerListener(){
  return database.ref(CUSTOMERREFPATH);
}
