import React, {FC, useEffect} from 'react';
import { useDispatch, connect } from 'react-redux';

import { ChangeSelectedAction } from '../redux/examples';
import { ChangeDirtyAction } from '../redux/editor';

import { GcpFileAction } from '../redux/actions/shared-file'
import {
    useParams
  } from "react-router-dom";

interface dispatchTypes {
  getFile: (id) => any
}

const ViewSharedFile:FC<dispatchTypes> = (props) => {
    let { id } = useParams();

  const dispatch = useDispatch();

  useEffect(() => {
    props.getFile(id).then((file) => {
      const list = JSON.parse(file)
      const {state} = list
      dispatch({ ...new ChangeSelectedAction(state) });
      dispatch({ ...new ChangeDirtyAction(true) });
    })
  }, [props, dispatch, id]);

  return (
    <></>
  );
};

const mapDispatchToProps = dispatch => {
  return({
    getFile: (id)  => dispatch(GcpFileAction(id))
  })
}

export default connect(null, mapDispatchToProps)(ViewSharedFile)