/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.jena.geosparql.spatial.filter_functions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.geosparql.configuration.GeoSPARQLConfig;
import org.apache.jena.geosparql.spatial.SpatialIndexTestData;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.sparql.expr.NodeValue;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 *
 */
public class AngleFFTest {

    public AngleFFTest() {
    }

    @BeforeClass
    public static void setUpClass() {
        GeoSPARQLConfig.setupNoIndex();
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    /**
     * Test of exec method, of class AngleFF.
     */
    @Test
    public void testExec() {

        NodeValue v1 = NodeValue.makeDouble(25);
        NodeValue v2 = NodeValue.makeDouble(45);
        NodeValue v3 = NodeValue.makeDouble(75);
        NodeValue v4 = NodeValue.makeDouble(100);
        AngleFF instance = new AngleFF();
        NodeValue expResult = NodeValue.makeDouble(0.7378150601204649);
        NodeValue result = instance.exec(v1, v2, v3, v4);
        assertEquals(expResult, result);
    }

    /**
     * Test of exec method, of class AngleFF.
     */
    @Test
    public void testExec_query() {
        Dataset dataset = SpatialIndexTestData.createTestDataset();
        String query = String.join("\n"
                                  ,"PREFIX spatialF: <http://jena.apache.org/function/spatial#>"
                                  , ""
                                  , "SELECT ?rads"
                                  , "WHERE{"
                                  , "    BIND( spatialF:angle(25, 45, 75, 100) AS ?rads)"
                                  , "}"
                                  ,"ORDER by ?rads"
                                  );

        List<Literal> results = new ArrayList<>();
        try (QueryExecution qe = QueryExecutionFactory.create(query, dataset)) {
            ResultSet rs = qe.execSelect();
            while (rs.hasNext()) {
                QuerySolution qs = rs.nextSolution();
                Literal result = qs.getLiteral("rads");
                results.add(result);
            }
        }

        List<Literal> expResults = Arrays.asList(ResourceFactory.createTypedLiteral("0.7378150601204649e0", XSDDatatype.XSDdouble));
        assertEquals(expResults, results);
    }

}
