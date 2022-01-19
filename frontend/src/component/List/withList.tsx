import React, { useState } from "react";
import useStyles from "./styles";
import Pagination from "./../../model/Pagination/Pagination";
import Filter from "./../../model/Filter/Filter";
import Modal from "./../Modal/Modal";
import { CircularProgress, Container, Grid, Fab } from '@material-ui/core'
import AddIcon from '@material-ui/icons/Add';
import DeleteIcon from '@material-ui/icons/Delete';
import useInfiniteScrolling from "../../hooks/useInifiniteScrolling";

export default function withList(Component: React.ComponentType, getPage: (handleNewList: ((newsList: any[]) => void), pagination: Pagination, filter: Filter) => void,
    renderTitles: () => JSX.Element, renderJurisdiction: (list: any[]) => JSX.Element | null, title: string, onDelete: () => void) {
    return function ComponentWithLayout() {
        const PAGE_SIZE = 20;

        const classes = useStyles();

        const [loading, , , , list] = useInfiniteScrolling(getPage, new Filter(), PAGE_SIZE)
        const [isModalOpen, setIsModalOpen] = useState(false);


        const onClose = () => { setIsModalOpen(false) }
        const onOpen = () => { setIsModalOpen(true) }

        return (
            <>
                <Container maxWidth={false} className={classes.root}>
                    <Grid container direction="column" className={classes.gridContainer} spacing={3}>
                        <Grid item>{renderTitles()}</Grid>
                        {list.map(item => <Grid item>{renderJurisdiction(item)}</Grid>)}
                    </Grid>
                    <Grid container direction="column" className={classes.gridContainer} spacing={3}>
                        <Grid item>
                            <Grid container justify="center" className={classes.gridContainer} spacing={3}>
                                {loading && <Grid item><CircularProgress color="primary" size={200} /></Grid>}
                            </Grid>
                        </Grid>
                    </Grid>
                </Container>
                <div className={classes.fab}>
                    <Grid container direction="column" className={classes.gridContainer} spacing={3}>
                        <Grid item>
                            <Fab color="primary" aria-label="add" onClick={onDelete}>
                                <DeleteIcon />
                            </Fab>
                        </Grid>
                        <Grid item>
                            <Fab color="primary" aria-label="add" onClick={onOpen}>
                                <AddIcon />
                            </Fab>
                        </Grid>
                    </Grid>
                </div>
                <Modal isOpen={isModalOpen} onClose={onClose} title={title}>
                    <Component />
                </Modal>
            </>
        );
    }
}
