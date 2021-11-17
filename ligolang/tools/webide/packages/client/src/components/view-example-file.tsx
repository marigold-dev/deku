import React, { FC, useEffect } from 'react';
import { connect } from 'react-redux';

import { ChangeSelectedAction } from '../redux/examples';
import { ExampleAction } from '../redux/actions/examples'

interface stateTypes {
  exampleId: any,
  isEditorDirty: boolean
}

interface dispatchTypes {
  setFile: (id) => any,
  getExample: (id) => any
}

const ViewExampleFile:FC<stateTypes&dispatchTypes> = (props) => {
  const { getExample, exampleId, setFile, isEditorDirty } = props

  useEffect(() => {
    if(exampleId && !isEditorDirty) {
    getExample(exampleId).then((list) => {
      setFile(list)
    })
   }
  },[exampleId, getExample, setFile, isEditorDirty])

  return (
    <></>
  );
};

const mapStateToProps = state => {
  const { examples, editor } = state
  return { 
    exampleId: examples && examples.list.length > 0 && examples.list[0].id,
    isEditorDirty : editor.dirty
   }
}

const mapDispatchToProps = dispatch => {
  return({
    setFile: (id)  => dispatch({ ...new ChangeSelectedAction(id)}),
    getExample: (id)  => dispatch(ExampleAction(id))
  })
}

export default connect(mapStateToProps, mapDispatchToProps)(ViewExampleFile)