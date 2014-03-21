A simple RESTful service
========================

.. http:get:: /info

   Get information about the application.

   **Example response**:

   .. sourcecode:: json

      {
        "name": "app-name",
        "version": "1.0",
        "plugins": [{
            "name": "cache",
            "version": "0.5.1"
        }, {
            "name": "i18n",
            "version": "4.1"
        }]
      }

   :resheader Content-Type: ``application/json``
   :status 200: always


.. http:get:: /(string:model)

   Get data for a given `model`.

   **Example request**:

   .. sourcecode:: http

      GET /user?sort=age&direction=desc&limit=3

   **Example response**:

   .. sourcecode:: json

      {
          "total": 5,
          "rows": [{
              "id": 10,
              "name": "John Doe",
              "age": 50
          }, {
              "id": 99,
              "name": "Jane Doe",
              "age": 48
          }, {
              "id": 112,
              "name": "Mark Doe",
              "age": 12
          }]
      }

   :query sort: by which property to sort
   :query direction: sort direction. one of: ``asc``, ``desc``
   :query limit: how many rows to get. default is 20
   :resheader Content-Type: ``application/json``
   :status 200: always


.. http:put:: /(string:model)/(int:id)

   Create a new `model` with a given `id`.

   **Example request**:

   .. sourcecode:: http

      PUT /user/201

      name="Ashley Doe"&age=23

   **Example response**:

   .. sourcecode:: http

      HTTP/1.1 201 CREATED

   :form property1: some property of a given `model`
   :form property2: another property of a given `model`
   :status 201 CREATED: if it was succesfully created
   :status 409 CONFLICT: if such a model already exists
   :status 412 PRECONDITION FAILED: if a validation fails
