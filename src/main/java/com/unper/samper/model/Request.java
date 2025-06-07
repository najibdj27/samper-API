package com.unper.samper.model;

import java.util.Date;

import javax.annotation.Nullable;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.unper.samper.model.common.Audit;
import com.unper.samper.model.constant.ERequestType;
import com.unper.samper.model.constant.EStatus;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "request", schema = "schedule")
public class Request extends Audit{
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String reason;

    private Date requestTime;

    private Date approveTime;

    private Boolean isApproved;

    @Enumerated(EnumType.STRING)
    private ERequestType type;

    @Enumerated(EnumType.STRING)
    private EStatus status;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = Schedule.class)
    @JoinColumn(name = "schedule_id")
    private Schedule schedule;

    @Nullable
    private String requestData;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = User.class)
    @JoinColumn(name = "sender_id")
    private User sender;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = User.class)
    @JoinColumn(name = "receiver_id")
    private User receiver;

    @Builder.Default
    private Boolean isDeleted = Boolean.FALSE;
}
