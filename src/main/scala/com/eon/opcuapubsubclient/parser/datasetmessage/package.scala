package com.eon.opcuapubsubclient.parser

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMetaData
import com.eon.opcuapubsubclient.parser.OpcUAPubSubParser.ParsePosition
import scodec.bits.ByteVector

// TODO: Under implementation
package object datasetmessage {

  def parseVariant = ???

  def parseRawFields(dataSetMetaData: DataSetMetaData, fieldCount: Int, byteVector: ByteVector, pos: ParsePosition) = {
    // Iterate over the fieldCount
    (0 until fieldCount) foreach {
      case fieldIndex =>
        // Get the FieldMetaData for the given fieldIndex
        val fieldMetaData = dataSetMetaData.fields(fieldIndex)

        // Get the StructureDataType from the DataSetMetaData and iterate over it
        val structureDataTypes = dataSetMetaData.dataTypeSchemaHeader.structureDataTypes
        structureDataTypes.foreach {
          case structureDataType =>
            if (structureDataType.dataTypeId.namespaceIndex == fieldMetaData.dataType.namespaceIndex &&
              structureDataType.dataTypeId.identifier.asString == fieldMetaData.dataType.identifier.asString) {

            }
        }
    }
  }

  def parseDataValue = ???
}
