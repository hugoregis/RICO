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

package org.apache.jena.shacl.validation.event;

import org.apache.jena.graph.Node;
import org.apache.jena.shacl.engine.ValidationContext;
import org.apache.jena.shacl.parser.Constraint;
import org.apache.jena.shacl.parser.Shape;

import java.util.Set;

/**
 * Event resulting from evaluating a constraint on the focus node, using compare nodes.
 */
public class ConstraintEvaluatedOnFocusNodeWithCompareNodesEvent extends ConstraintEvaluatedOnFocusNodeEvent
                implements CompareNodesEvent {
    protected final ImmutableLazySetCopy<Node> compareNodes;

    public ConstraintEvaluatedOnFocusNodeWithCompareNodesEvent(ValidationContext vCxt,
                    Shape shape, Node focusNode,
                    Constraint constraint, Set<Node> compareNodes, boolean valid) {
        super(vCxt, shape, focusNode, constraint, valid);
        this.compareNodes = new ImmutableLazySetCopy<>(compareNodes);
    }

    @Override public Set<Node> getCompareNodes() {
        return compareNodes.get();
    }

    @Override public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        if (!super.equals(o))
            return false;
        ConstraintEvaluatedOnFocusNodeWithCompareNodesEvent that = (ConstraintEvaluatedOnFocusNodeWithCompareNodesEvent) o;
        return getCompareNodes().equals(that.getCompareNodes());
    }

    @Override public int hashCode() {
        int result = super.hashCode();
        result = 31 * result + getCompareNodes().hashCode();
        return result;
    }

    @Override public String toString() {
        return "ConstraintEvaluatedOnFocusNodeWithCompareNodesEvent{" +
                        "constraint=" + constraint +
                        ", focusNode=" + focusNode +
                        ", shape=" + shape +
                        ", valid=" + valid +
                        ", compareNodes=" + compareNodes +
                        '}';
    }
}
